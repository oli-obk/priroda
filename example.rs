fn some_fn() {
    let mut val = 2;
    let mut my_wrapper = MyWrapper(&mut val);
    let wrapper_ref = &mut my_wrapper;
    for i in 0..16 {
        *wrapper_ref.0 = i;
    }
}

struct MyWrapper<'a>(&'a mut u8);

struct SomeRandomStruct {
    ernrer: u8,
    feijoc: u64,
    ioieoe: bool,
    fewije: char,
}

fn main() -> Result<(), std::io::Error> {
    let _rer = SomeRandomStruct {
        ernrer: 24,
        feijoc: 34438,
        ioieoe: true,
        fewije: '@',
    };
    let abc = Box::new(0);
    some_fn();
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
