/// Location referencing as defined in ISO 14819-3:2013
use crate::location::exchange::{
    CountryCode, CountryId, ExtendedCountryCode, LanguageId, LocationCode, NameId, TableCode,
};
use crate::Error;
use log::{debug, trace, warn};
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter, Write};
use std::io::ErrorKind;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::sync::Arc;

mod exchange;

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum Category {
    Area,
    Linear,
    Point,
}

impl From<Category> for char {
    fn from(category: Category) -> Self {
        match category {
            Category::Area => 'A',
            Category::Linear => 'L',
            Category::Point => 'P',
        }
    }
}

impl Display for Category {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_char((*self).into())
    }
}

impl FromStr for Category {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "A" => Self::Area,
            "L" => Self::Linear,
            "P" => Self::Point,
            _ => return Err("Invalid category".into()),
        })
    }
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub struct Subtype(Category, u8, u8);

impl Subtype {
    pub fn category(self) -> Category {
        self.0
    }

    pub fn r#type(self) -> u8 {
        self.1
    }

    pub fn subtype(self) -> u8 {
        self.2
    }
}

impl Display for Subtype {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}.{}", self.0, self.1, self.2)
    }
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub struct QualifiedCode(CountryCode, TableCode, LocationCode);

impl QualifiedCode {
    pub fn new(
        country: CountryCode,
        table: TableCode,
        location: LocationCode,
    ) -> Result<Self, Error> {
        if country > 0xF {
            return Err("Invalid country code".into());
        }
        Ok(Self(country, table, location))
    }
}

impl From<ExtendedCode> for QualifiedCode {
    fn from(code: ExtendedCode) -> Self {
        Self(code.1, code.2, code.3)
    }
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub struct ExtendedCode(ExtendedCountryCode, CountryCode, TableCode, LocationCode);

impl ExtendedCode {
    pub fn new(
        extended_country: ExtendedCountryCode,
        country: CountryCode,
        table: TableCode,
        location: LocationCode,
    ) -> Result<Self, Error> {
        if country > 0xF {
            return Err("Invalid country code".into());
        }
        Ok(Self(extended_country, country, table, location))
    }
}

pub trait Area {
    type Error;

    fn code(&self) -> ExtendedCode;
    fn subtype(&self) -> Subtype;
    fn area(&self) -> Result<Option<Self>, Self::Error>
    where
        Self: Sized;
}

pub trait Point {
    type Area;
    type Error;

    fn code(&self) -> ExtendedCode;
    fn subtype(&self) -> Subtype;
    fn administrative_area(&self) -> Result<Option<Self::Area>, Self::Error>;
    fn other_area(&self) -> Result<Option<Self::Area>, Self::Error>;
    fn latitude(&self) -> f32;
    fn longitude(&self) -> f32;
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum Location<A, P, R, S> {
    Area(A),
    Point(P),
    Route(R),
    Segment(S),
}

impl<A, P, R, S> Location<A, P, R, S> {
    pub fn category(&self) -> Category {
        match self {
            Self::Area(_) => Category::Area,
            Self::Point(_) => Category::Point,
            Self::Route(_) => Category::Linear,
            Self::Segment(_) => Category::Linear,
        }
    }
}

impl<A, P, R, S> Location<A, P, R, S>
where
    A: Area,
    P: Point,
{
    pub fn code(&self) -> ExtendedCode {
        match self {
            Self::Area(area) => area.code(),
            Self::Point(point) => point.code(),
            _ => todo!(),
        }
    }

    pub fn subtype(&self) -> Subtype {
        match self {
            Self::Area(area) => area.subtype(),
            Self::Point(point) => point.subtype(),
            _ => todo!(),
        }
    }
}

pub trait Provider<Q, R> {
    fn query(&self, query: Q) -> R;
}

#[derive(Clone, Hash, Eq, PartialEq, Debug)]
pub struct AreaRow {
    pub code: ExtendedCode,
    pub subtype: Subtype,
    name: NameId,
    area: Option<LocationCode>,
}

#[derive(Clone, Hash, Eq, PartialEq, Debug)]
pub struct PointRow {
    pub code: ExtendedCode,
    pub subtype: Subtype,
    pub junction: Option<String>,
    road_name: Option<NameId>,
    name1: Option<NameId>,
    name2: Option<NameId>,
    administrative_area: Option<LocationCode>,
    other_area: Option<LocationCode>,
    segment: Option<LocationCode>,
    road: Option<LocationCode>,
    pub in_pos: bool,
    pub in_neg: bool,
    pub out_pos: bool,
    pub out_neg: bool,
    pub present_pos: bool,
    pub present_neg: bool,
    pub diversion_pos: Option<String>,
    pub diversion_neg: Option<String>,
    pub x: i32,
    pub y: i32,
    interrupted_road: Option<LocationCode>,
    pub urban: bool,
    pub offset_neg: Option<LocationCode>,
    pub offset_pos: Option<LocationCode>,
}

impl PointRow {
    fn longitude(&self) -> f32 {
        self.x as f32 / 100000.0
    }
    fn latitude(&self) -> f32 {
        self.y as f32 / 100000.0
    }
}

pub struct DefaultProvider {
    tables: HashMap<(u8, u8), Arc<Tables>>,
}

impl DefaultProvider {
    pub fn new() -> Self {
        Self {
            tables: HashMap::new(),
        }
    }

    pub fn add(&mut self, tables: Tables) -> &mut Self {
        self.tables
            .insert((tables.country, tables.table), Arc::new(tables));
        self
    }

    pub fn update(&mut self, tables: Tables) -> &mut Self {
        match self.tables.entry((tables.country, tables.table)) {
            Entry::Occupied(mut entry) => {
                let existing = entry.get();
                if existing.major < tables.major
                    || (existing.major == tables.major && existing.minor < tables.minor)
                {
                    entry.insert(Arc::new(tables));
                }
            }
            Entry::Vacant(entry) => {
                entry.insert(Arc::new(tables));
            }
        }
        self
    }

    pub fn areas(&self) -> impl Iterator<Item = AreaRef> + '_ {
        self.tables
            .values()
            .map(|tables| {
                tables
                    .administrative_areas
                    .iter()
                    .map(move |(country, area)| {
                        AreaRef(Arc::clone(&tables), country.0, Arc::clone(&area))
                    })
                    .chain(tables.other_areas.iter().map(move |(country, area)| {
                        AreaRef(Arc::clone(&tables), country.0, Arc::clone(&area))
                    }))
            })
            .flatten()
    }

    pub fn points(&self) -> impl Iterator<Item = PointRef> + '_ {
        self.tables
            .values()
            .map(|tables| {
                tables.points.iter().map(move |(country, point)| {
                    PointRef(Arc::clone(&tables), country.0, Arc::clone(&point))
                })
            })
            .flatten()
    }

    fn query_by_code(&self, code: QualifiedCode) -> Option<Location<AreaRef, PointRef, (), ()>> {
        for tables in self.tables.values() {
            let country = if let Some(country) = tables.country_ids.get(&code.0) {
                *country
            } else {
                continue;
            };
            let key = Tables::into_key(code, country, None);
            trace!("Search for {:?}", key);
            if let Some(result) = tables.administrative_areas.get(&key) {
                return Some(Location::Area(AreaRef(
                    Arc::clone(&tables),
                    country,
                    Arc::clone(result),
                )));
            }
            if let Some(result) = tables.other_areas.get(&key) {
                return Some(Location::Area(AreaRef(
                    Arc::clone(&tables),
                    country,
                    Arc::clone(result),
                )));
            }
            if let Some(result) = tables.points.get(&key) {
                return Some(Location::Point(PointRef(
                    Arc::clone(&tables),
                    country,
                    Arc::clone(result),
                )));
            }
        }
        None
    }
}

impl From<Tables> for DefaultProvider {
    fn from(tables: Tables) -> Self {
        Some(tables).into()
    }
}

impl<I> From<I> for DefaultProvider
where
    I: IntoIterator<Item = Tables>,
{
    fn from(iter: I) -> Self {
        let mut result = DefaultProvider::new();
        for tables in iter {
            result.update(tables);
        }
        result
    }
}

impl Provider<QualifiedCode, Option<Location<AreaRef, PointRef, (), ()>>> for DefaultProvider {
    fn query(&self, code: QualifiedCode) -> Option<Location<AreaRef, PointRef, (), ()>> {
        self.query_by_code(code)
    }
}

impl Provider<ExtendedCode, Option<Location<AreaRef, PointRef, (), ()>>> for DefaultProvider {
    fn query(&self, code: ExtendedCode) -> Option<Location<AreaRef, PointRef, (), ()>> {
        self.query_by_code(code.into())
    }
}

#[derive(Clone)]
pub struct AreaRef(Arc<Tables>, CountryId, Arc<AreaRow>);

impl AreaRef {
    pub fn name(&self) -> Option<&str> {
        self.0.names.get(&self.2.name).map(|s| s.2.as_ref())
    }
}

impl Area for AreaRef {
    type Error = Error;

    fn code(&self) -> ExtendedCode {
        self.as_ref().code
    }

    fn subtype(&self) -> Subtype {
        self.as_ref().subtype
    }

    fn area(&self) -> Result<Option<Self>, Self::Error> {
        if let Some(area) = self.2.area {
            let area = self
                .0
                .administrative_areas
                .get(&Tables::into_key(self.2.code, self.1, area))
                .ok_or("Invalid administrative area")?;
            Ok(Some(Self(Arc::clone(&self.0), self.1, Arc::clone(&area))))
        } else {
            Ok(None)
        }
    }
}

impl AsRef<AreaRow> for AreaRef {
    fn as_ref(&self) -> &AreaRow {
        &self.2
    }
}

impl Debug for AreaRef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("AreaRef")
            .field(&Arc::as_ptr(&self.0))
            .field(&self.1)
            .field(&self.2)
            .finish()
    }
}

#[derive(Clone)]
pub struct PointRef(Arc<Tables>, CountryId, Arc<PointRow>);

impl Point for PointRef {
    type Area = AreaRef;
    type Error = Error;

    fn code(&self) -> ExtendedCode {
        self.as_ref().code
    }

    fn subtype(&self) -> Subtype {
        self.as_ref().subtype
    }

    fn administrative_area(&self) -> Result<Option<AreaRef>, Error> {
        if let Some(area) = self.2.administrative_area {
            let area = self
                .0
                .administrative_areas
                .get(&Tables::into_key(self.2.code, self.1, area))
                .ok_or("Invalid administrative area")?;
            Ok(Some(AreaRef(
                Arc::clone(&self.0),
                self.1,
                Arc::clone(&area),
            )))
        } else {
            Ok(None)
        }
    }

    fn other_area(&self) -> Result<Option<AreaRef>, Error> {
        if let Some(area) = self.2.other_area {
            let area = self
                .0
                .other_areas
                .get(&Tables::into_key(self.2.code, self.1, area))
                .ok_or("Invalid other area")?;
            Ok(Some(AreaRef(
                Arc::clone(&self.0),
                self.1,
                Arc::clone(&area),
            )))
        } else {
            Ok(None)
        }
    }

    fn latitude(&self) -> f32 {
        self.as_ref().latitude()
    }

    fn longitude(&self) -> f32 {
        self.as_ref().longitude()
    }
}

impl AsRef<PointRow> for PointRef {
    fn as_ref(&self) -> &PointRow {
        &self.2
    }
}

impl Debug for PointRef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("PointRef")
            .field(&Arc::as_ptr(&self.0))
            .field(&self.1)
            .field(&self.2)
            .finish()
    }
}

#[derive(Debug)]
pub struct Tables {
    extended_country: u8,
    country: u8,
    table: u8,
    major: u8,
    minor: u8,
    administrative_areas: HashMap<(CountryId, TableCode, LocationCode), Arc<AreaRow>>,
    countries: HashMap<CountryId, (ExtendedCountryCode, CountryCode, String)>,
    names: HashMap<NameId, (CountryId, LanguageId, String)>,
    languages: HashMap<(CountryId, LanguageId), String>,
    other_areas: HashMap<(CountryId, TableCode, LocationCode), Arc<AreaRow>>,
    points: HashMap<(CountryId, TableCode, LocationCode), Arc<PointRow>>,
    country_ids: HashMap<CountryCode, CountryId>,
    extended_country_ids: HashMap<(ExtendedCountryCode, CountryCode), CountryId>,
}

impl Tables {
    pub fn load(directory: impl AsRef<Path>) -> Result<Self, Error> {
        Self::load_with(|table| {
            let mut path = PathBuf::from(directory.as_ref());
            path.push(table.as_code());
            path.set_extension("DAT");
            debug!("Read {:?}", path);
            match std::fs::read(path) {
                Ok(data) => Ok(Some(data)),
                Err(err) if err.kind() == ErrorKind::NotFound => Ok(None),
                Err(err) => Err(err),
            }
        })
    }

    pub fn load_with<F, T, E>(mut loader: F) -> Result<Self, Error>
    where
        F: FnMut(Table) -> Result<Option<T>, E>,
        E: Into<Error>,
        T: AsRef<[u8]>,
    {
        let charset = loader(Table::MetaInformation)
            .map_err(|err| err.into())?
            .and_then(|data| exchange::parse_meta(data.as_ref()))
            .unwrap_or_default();
        let mut loader = |table| {
            loader(table)
                .map(|result| result.map(|data| charset.decode_owned(data)))
                .map_err(|err| err.into())
        };

        let (areas, _linears, points) = if let Some(table) = loader(Table::Classes)? {
            let classes = exchange::parse_classes(&table)?;
            (
                classes.contains(&Category::Area),
                classes.contains(&Category::Linear),
                classes.contains(&Category::Point),
            )
        } else {
            (true, true, true)
        };

        let countries = loader(Table::Countries)?.ok_or("Missing countries table")?;
        let countries = exchange::parse_countries(&countries)?;
        let country_codes = |id| countries.get(&id).map(|c| (c.0, c.1));
        let country_ids = countries
            .iter()
            .map(|(id, (_, code, _))| (*code, *id))
            .collect::<HashMap<_, _>>();
        let extended_country_ids = countries
            .iter()
            .map(|(id, (extended, code, _))| ((*extended, *code), *id))
            .collect::<HashMap<_, _>>();
        if country_ids.len() < countries.len() || extended_country_ids.len() < countries.len() {
            warn!("Multiple countries use the same country code");
        }

        let datasets = loader(Table::LocationDatasets)?.ok_or("Missing datasets table")?;
        let dataset = exchange::parse_datasets(&datasets)?;
        let dataset_country = countries
            .get(&dataset.0)
            .ok_or("Invalid country in dataset")?;

        let languages = loader(Table::Languages)?.ok_or("Missing languages table")?;
        let languages = exchange::parse_languages(&languages)?;

        let names = loader(Table::Names)?.ok_or("Missing names table")?;
        let names = exchange::parse_names(&names)?;

        let (administrative_areas, other_areas) = if areas {
            let administrative_areas =
                loader(Table::AdministrativeAreas)?.ok_or("Missing administrative areas table")?;
            let administrative_areas = exchange::parse_areas(&administrative_areas, country_codes)?;
            let other_areas = loader(Table::OtherAreas)?.ok_or("Missing other areas table")?;
            let other_areas = exchange::parse_areas(&other_areas, country_codes)?;
            (administrative_areas, other_areas)
        } else {
            (HashMap::new(), HashMap::new())
        };

        let points = if points {
            let points = loader(Table::Points)?.ok_or("Missing points table")?;
            let mut points = exchange::parse_points(&points, country_codes)?;
            if let Some(poffsets) = loader(Table::PointOffsets)? {
                exchange::parse_poffsets_into(&poffsets, &mut points)?;
            }
            points
        } else {
            HashMap::new()
        };

        Ok(Self {
            extended_country: dataset_country.0,
            country: dataset_country.1,
            table: dataset.1,
            major: dataset.2 .0,
            minor: dataset.2 .1,
            administrative_areas,
            countries,
            languages,
            names,
            other_areas,
            points,
            country_ids,
            extended_country_ids,
        })
    }

    fn into_key(
        qualified: impl Into<QualifiedCode>,
        country: CountryId,
        code: impl Into<Option<LocationCode>>,
    ) -> (CountryId, TableCode, LocationCode) {
        let qualified = qualified.into();
        (country, qualified.1, code.into().unwrap_or(qualified.2))
    }
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum Table {
    AdministrativeAreas,
    Classes,
    Countries,
    EuroRoadCountries,
    EuroRoadNumbers,
    Intersections,
    Languages,
    LocationCodes,
    LocationDatasets,
    Names,
    NameTranslations,
    OtherAreas,
    PointOffsets,
    Points,
    Roads,
    EuroRoadSegments,
    Segments,
    SegmentOffsets,
    Subtypes,
    SubtypeTranslations,
    Types,
    RoadNetworkLevelTypes,
    MetaInformation,
}

impl Table {
    fn as_code(self) -> &'static str {
        match self {
            Self::AdministrativeAreas => "ADMINISTRATIVEAREA",
            Self::Classes => "CLASSES",
            Self::Countries => "COUNTRIES",
            Self::EuroRoadCountries => "ERNO_BELONGS_TO_CO",
            Self::EuroRoadNumbers => "EU ROROADNO",
            Self::Intersections => "INTERSECTIONS",
            Self::Languages => "LANGUAGES",
            Self::LocationCodes => "LOCATIONCODES",
            Self::LocationDatasets => "LOCATIONDATASETS",
            Self::Names => "NAMES",
            Self::NameTranslations => "NAMETRANSLATIONS",
            Self::OtherAreas => "OTHERAREAS",
            Self::PointOffsets => "POFFSETS",
            Self::Points => "POINTS",
            Self::Roads => "ROADS",
            Self::EuroRoadSegments => "SEG_HAS_ERNO",
            Self::Segments => "SEGMENTS",
            Self::SegmentOffsets => "SOFFSETS",
            Self::Subtypes => "SUBTYPES",
            Self::SubtypeTranslations => "SUBTYPETRANSLATION",
            Self::Types => "TYPES",
            Self::RoadNetworkLevelTypes => "ROAD_NETWORK_LEVEL_TYPES",
            Self::MetaInformation => "README",
        }
    }
}
