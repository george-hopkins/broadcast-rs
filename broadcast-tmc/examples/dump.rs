use broadcast_tmc::location::{Area, DefaultProvider, Point, Tables};
use broadcast_tmc::Error;

fn main() -> Result<(), Error> {
    pretty_env_logger::init();
    let directory = std::env::args()
        .skip(1)
        .next()
        .ok_or("Path to directory is missing")?;
    let tables = Tables::load(directory)?;
    let provider = DefaultProvider::from(tables);
    for area in provider.areas() {
        println!("{:?} {} {:?}", area.code(), area.subtype(), area.name());
    }
    for point in provider.points() {
        println!(
            "{:?} {} {} {}",
            point.code(),
            point.subtype(),
            point.latitude(),
            point.longitude()
        );
    }
    Ok(())
}
