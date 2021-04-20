use broadcast_tmc::location::{DefaultProvider, Provider, QualifiedCode, Tables};
use broadcast_tmc::Error;

fn main() -> Result<(), Error> {
    pretty_env_logger::init();

    let mut args = std::env::args().skip(1);
    let directory = args.next().ok_or("Path to directory is missing")?;
    let country = args.next().ok_or("Country code is missing")?.parse()?;
    let table = args.next().ok_or("Table number is missing")?.parse()?;
    let location = args.next().ok_or("Location code is missing")?.parse()?;

    let tables = Tables::load(directory)?;
    let provider = DefaultProvider::from(tables);
    let query = QualifiedCode::new(country, table, location)?;
    println!("{:?}", provider.query(query));

    Ok(())
}
