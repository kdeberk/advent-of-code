
const SIZE:usize = 300;
const SERIAL:usize = 8772;

fn part_1(grid: &[[i8; SIZE]; SIZE]) {
    let mut max = std::i32::MIN;
    let mut max_coordinate = (0, 0);

    for x in 0..SIZE - 2 {
        for y in 0..SIZE - 2 {
             let mut sum:i32 = 0;

            for xx in x..x + 3 {
                for yy in y..y + 3 {
                    sum += grid[yy][xx] as i32;
                }
            }

            if sum > max {
                max = sum;
                max_coordinate = (x, y);
            }
        }
    }
    println!("{},{}", max_coordinate.0, max_coordinate.1);
}


fn part_2(grid: &[[i8; SIZE]; SIZE]) {
    let mut max = std::i32::MIN;
    let mut max_coordinate = (0, 0, 0);

    for x in 0..SIZE - 2 {
        for y in 0..SIZE - 2 {
            let mut grid_sum:i32 = 0;

            for size in 0..(SIZE - std::cmp::max(x, y)) {
                for xx in x..x + size {
                    grid_sum += grid[y + size][xx] as i32;
                }
                for yy in y..y + size {
                    grid_sum += grid[yy][x + size] as i32;
                }
                grid_sum += grid[y + size][x + size] as i32;

                if grid_sum > max {
                    max = grid_sum;
                    max_coordinate = (x, y, size + 1);
                }
            }
        }
    }
    println!("{},{},{}", max_coordinate.0, max_coordinate.1, max_coordinate.2);
}


fn main() {
    let mut grid:[[i8; SIZE]; SIZE] = [[0; SIZE]; SIZE];

    for x in 0..SIZE {
        let rack_id = 10 + x;

        for y in 0..SIZE {
            let mut power_level = rack_id * (rack_id * y + SERIAL);

            grid[y][x] = ((power_level % 1_000) / 100) as i8;
            grid[y][x] -= 5;
        }
    }


    part_1(&grid);
    part_2(&grid);
}
