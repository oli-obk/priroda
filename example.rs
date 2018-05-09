fn return_42() -> u64 {
    42
}

struct MyWrapper(&'static u8);

fn main() -> Result<(), std::io::Error> {
    let my_wrapper = MyWrapper(&2);
    let _wrapper_ref = &my_wrapper;
    let abc = Box::new(0);
    return_42();
    let f = 1.4f64;
    let _sum = 1f64
    +
    f;
    let _s = "ieeoe";
    let _bcd: Box<[u8]> = Box::new([0, 1]);
    let _d = true;
    format!("ewioio: {}", abc);
    Ok(())
}
