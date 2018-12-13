
fn part_1() {
    let factor_a = 16807;
    let factor_b = 48271;

    let mut count = 0;
    let mut current_a:u64 = 883;
    let mut current_b:u64 = 879;
    for _ in 0..40_000_000 {
        if current_a & 0xffff == current_b & 0xffff {
            count += 1
        }
        
        current_a = (current_a * factor_a) % 0x7fffffff;
        current_b = (current_b * factor_b) % 0x7fffffff;
    }

    println!("{}", count);
}

fn part_2() {
    let factor_a = 16807;
    let factor_b = 48271;

    let mut count = 0;
    let mut current_a:u64 = 883;
    let mut current_b:u64 = 879;
    for _ in 0..5_000_000 {
        if current_a & 0xffff == current_b & 0xffff {
            count += 1
        }
        
        current_a = (current_a * factor_a) % 0x7fffffff;
        while 0 != current_a % 4 {
            current_a = (current_a * factor_a) % 0x7fffffff;
        }
        current_b = (current_b * factor_b) % 0x7fffffff;
        while 0 != current_b % 8 {
            current_b = (current_b * factor_b) % 0x7fffffff;
        }
    }

    println!("{}", count);
}

fn main() {
    part_1();
    part_2();
}
