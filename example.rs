#![feature(termination_trait)]

fn return_42() -> u64 {
    42
}

fn main() -> Result<(), std::io::Error> {
    let abc = Box::new(0);
    return_42();
    let f = 1.4f64;
    let sum = 1f64
    +
    f;
    let s = "ieeoe";
    let bcd: Box<[u8]> = Box::new([0, 1]);
    let d = true;
    format!("ewioio: {}", abc);
    Ok(())
}
