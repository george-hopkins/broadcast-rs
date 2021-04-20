use super::{AreaRow, Category, ExtendedCode, PointRow, Subtype};
use crate::Error;
use encoding_rs::{ISO_8859_15, UTF_8, WINDOWS_1252};
use log::warn;
use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::sync::Arc;

pub type CountryCode = u8;
pub type CountryId = usize;
pub type ExtendedCountryCode = u8;
pub type LanguageId = usize;
pub type LocationCode = u16;
pub type NameId = usize;
pub type TableCode = u8;

pub enum Charset {
    Utf8,
    Latin1,
    Latin9,
}

impl Default for Charset {
    fn default() -> Self {
        Charset::Utf8
    }
}

impl Charset {
    pub fn decode<'a>(&self, data: &'a [u8]) -> Cow<'a, str> {
        match self {
            Charset::Utf8 => UTF_8.decode(data).0,
            Charset::Latin1 => WINDOWS_1252.decode(data).0,
            Charset::Latin9 => ISO_8859_15.decode(data).0,
        }
    }

    pub fn decode_owned(&self, data: impl AsRef<[u8]>) -> String {
        self.decode(data.as_ref()).into_owned()
    }
}

macro_rules! parse_table {
    ($data:expr, [$($field:tt: $column:literal),*$(,)?], $consume:tt) => {
        {
            let mut lines = { $data }.lines().map(|l| l.split(';'));
            let header: Vec<_> = lines.next().unwrap().collect();
            $(
            let parse_table!(@var $field) = header.iter().position(|h| *h == $column);
            parse_table!(@header $field $column);
            )*
            for (i, line) in lines.enumerate() {
                let line: Vec<_> = line.collect();
                $(
                parse_table!(@get $field line);
                let parse_table!(@var $field) = parse_table!(@var $field)
                    .map(|v| v.trim())
                    .filter(|v| !v.is_empty())
                    .map(::std::str::FromStr::from_str)
                    .transpose()
                    .map_err(|e| format!("Invalid value in column {} on line {}: {}", $column, i + 2, e))?;
                parse_table!(@require $field $column i);
                )*
                $consume
            }
        }
    };
    (@var $field:ident) => { $field };
    (@var [$field:ident]) => { $field };
    (@header $field:ident $column:literal) => { let $field = $field.ok_or(concat!("Missing column ", $column))?; };
    (@header [$field:ident] $column:literal) => {};
    (@get $field:ident $line:ident) => { let $field = $line.get($field); };
    (@get [$field:ident] $line:ident) => { let $field = $field.and_then(|i| $line.get(i)); };
    (@require $field:ident $column:literal $line:ident) => { let $field = $field.ok_or(format!("Missing value in column {} on line {}", $column, $line + 2))?; };
    (@require [$field:ident] $column:literal $line:ident) => {};
}

pub fn parse_areas<F>(
    table: &str,
    mut countries: F,
) -> Result<HashMap<(CountryId, TableCode, LocationCode), Arc<AreaRow>>, Error>
where
    F: FnMut(CountryId) -> Option<(ExtendedCountryCode, CountryCode)>,
{
    let mut areas = HashMap::new();
    parse_table!(table, [
        country: "CID",
        table: "TABCD",
        code: "LCD",
        class: "CLASS",
        r#type: "TCD",
        subtype: "STCD",
        name: "NID",
        [area]: "POL_LCD",
    ], {
        if class != Category::Area {
            continue;
        }
        let country_code = if let Some(country_code) = countries(country) {
            country_code
        } else {
            warn!("No country code");
            continue;
        };
        areas.insert((country, table, code), Arc::new(AreaRow {
            code: ExtendedCode(country_code.0, country_code.1, table, code),
            subtype: Subtype(class, r#type, subtype),
            name,
            area,
        }));
    });
    Ok(areas)
}

pub fn parse_classes(table: &str) -> Result<HashSet<Category>, Error> {
    let mut classes = HashSet::new();
    parse_table!(table, [
        class: "CLASS",
    ], {
        classes.insert(class);
    });
    Ok(classes)
}

pub fn parse_countries(
    table: &str,
) -> Result<HashMap<CountryId, (ExtendedCountryCode, CountryCode, String)>, Error> {
    let mut result = HashMap::new();
    parse_table!(table, [
        id: "CID",
        extended_code: "ECC",
        code: "CCD",
        name: "CNAME",
    ], {
        let extended_code = parse_hex_u8(String::as_str(&extended_code))?;
        let code = parse_hex_u8(String::as_str(&code))?;
        if code > 0xF {
            return Err(format!("Invalid country code {:X}", code).into());
        }
        result.insert(id, ( extended_code, code,name));
    });
    Ok(result)
}

pub fn parse_datasets(table: &str) -> Result<(CountryId, TableCode, (u8, u8)), Error> {
    let mut result = None;
    parse_table!(table, [
        country: "CID",
        table: "TABCD",
        version: "VERSION",
    ], {
        if result.is_some() {
            return Err("Multiple dataset descriptions".into());
        }
        let version = parse_version(String::as_str(&version))?;
        result = Some((country, table, version));
    });
    result.ok_or_else(|| "No dataset description".into())
}

pub fn parse_languages(table: &str) -> Result<HashMap<(CountryId, LanguageId), String>, Error> {
    let mut result = HashMap::new();
    parse_table!(table, [
        country: "CID",
        language: "LID",
        name: "LANGUAGE",
    ], {
        result.insert((country, language), name);
    });
    Ok(result)
}

pub fn parse_names(table: &str) -> Result<HashMap<NameId, (CountryId, LanguageId, String)>, Error> {
    let mut result = HashMap::new();
    parse_table!(table, [
        country: "CID",
        language: "LID",
        id: "NID",
        name: "NAME",
    ], {
        result.insert(id, (country, language, name));
    });
    Ok(result)
}

pub fn parse_meta(table: &[u8]) -> Option<Charset> {
    let text = String::from_utf8_lossy(table);
    let line = text.lines().next().unwrap();
    for value in line.split(';') {
        if value.eq_ignore_ascii_case("UTF-8") {
            return Some(Charset::Utf8);
        } else if value.eq_ignore_ascii_case("ISO-8859-1") {
            return Some(Charset::Latin1);
        } else if value.eq_ignore_ascii_case("ISO-8859-15") {
            return Some(Charset::Latin9);
        }
    }
    None
}

pub fn parse_points<F>(
    table: &str,
    mut countries: F,
) -> Result<HashMap<(CountryId, TableCode, LocationCode), Arc<PointRow>>, Error>
where
    F: FnMut(CountryId) -> Option<(ExtendedCountryCode, CountryCode)>,
{
    let mut result = HashMap::new();
    parse_table!(table, [
        country: "CID",
        table: "TABCD",
        code: "LCD",
        class: "CLASS",
        r#type: "TCD",
        subtype: "STCD",
        [junction]: "JUNCTIONNUMBER",
        [road_name]: "RNIO",
        [name1]: "NCR",
        [name2]: "NOD",
        [administrative_area]: "POL_LCO",
        [other_area]: "OTH_LCD",
        [segment]: "SEG_LCO",
        [road]: "ROA_LCD",
        in_pos: "INPOS",
        in_neg: "INNEG",
        out_pos: "OUTPOS",
        out_neg: "OUTNEG",
        present_pos: "PRESENTPOS",
        present_neg: "PRESENTNEG",
        [diversion_pos]: "DIVERSIONPOS",
        [diversion_neg]: "DIVERSIONNEG",
        x: "XCOORD",
        y: "YCOORD",
        [interrupted_road]: "INTERRUPTSROAD",
        urban: "URBAN",
    ], {
        if class != Category::Point {
            continue;
        }
        let country_code = if let Some(country_code) = countries(country) {
            country_code
        } else {
            warn!("No country code");
            continue;
        };
        result.insert((country, table, code), Arc::new(PointRow {
            code: ExtendedCode(country_code.0, country_code.1, table, code),
            subtype: Subtype(class, r#type, subtype),
            junction,
            road_name,
            name1,
            name2,
            administrative_area,
            other_area,
            segment,
            road,
            in_pos: 1u8 == in_pos,
            in_neg: 1u8 == in_neg,
            out_pos: 1u8 == out_pos,
            out_neg: 1u8 == out_neg,
            present_pos: 1u8 == present_pos,
            present_neg: 1u8 == present_neg,
            diversion_pos,
            diversion_neg,
            x,
            y,
            interrupted_road,
            urban: 0u8 != urban,
            offset_neg: None,
            offset_pos: None,
        }));
    });
    Ok(result)
}

pub fn parse_poffsets_into(
    table: &str,
    points: &mut HashMap<(CountryId, TableCode, LocationCode), Arc<PointRow>>,
) -> Result<(), Error> {
    parse_table!(table, [
        country: "CID",
        table: "TABCD",
        code: "LCD",
        [offset_neg]: "NEG_OFF_LCD",
        [offset_pos]: "POS_OFF_LCD",
    ], {
        if let Some(point) = points.get_mut(&(country, table, code)).and_then(Arc::get_mut) {
            point.offset_neg = offset_neg;
            point.offset_pos = offset_pos;
        } else {
            warn!("Offset for inexistent point {}", code);
        }
    });
    Ok(())
}

fn parse_version(version: &str) -> Result<(u8, u8), Error> {
    let mut version = version.split('.').map(|n| n.parse());
    match (version.next(), version.next(), version.next()) {
        (Some(Ok(major)), None, _) => Ok((major, 0)),
        (Some(Ok(major)), Some(Ok(minor)), None) => Ok((major, minor)),
        _ => Err(format!("Invalid version number: {:?}", version).into()),
    }
}

fn parse_hex_u8(hex: &str) -> Result<u8, Error> {
    let mut chars = hex.chars().map(|c| c.to_digit(16));
    Ok(match (chars.next(), chars.next(), chars.next()) {
        (None, _, _) => 0,
        (Some(Some(r)), None, _) => r as u8,
        (Some(Some(l)), Some(Some(r)), None) => (l << 4 | r) as u8,
        _ => return Err("Invalid hex string".into()),
    })
}

#[cfg(test)]
mod tests {
    use crate::Error;

    #[test]
    fn test_parse_table() {
        let f = |data: &str| -> Result<Vec<(_, _, _, Option<String>)>, Error> {
            let mut result = vec![];
            parse_table!(data, [a: "aa", b: "bb", [c]: "cc", [d]: "dd"], {
                result.push((a, b, c, d));
            });
            Ok(result)
        };

        let ok = &"bb;aa;cc\n1;2;3\n5;6"[..];
        let result = f(ok).unwrap();
        assert_eq!(2, result.len());
        assert_eq!((2, 1, Some(3), None), result[0]);
        assert_eq!((6, 5, None, None), result[1]);

        let err = &"bb;aa;cc\n4"[..];
        let result = f(err).unwrap_err();
        assert_eq!("Missing value in column aa on line 2", result.to_string());
    }
}
