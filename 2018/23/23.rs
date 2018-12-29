use std::fmt;
use std::fs::File;
use std::io::{BufRead, BufReader};

#[derive(Clone, Copy)]
struct Point {
    x: f64,
    y: f64,
    z: f64,
}

impl Point {
    pub fn d(a: &Point, b: &Point) -> usize {
        ((a.x - b.x).abs() + (a.y - b.y).abs() + (a.z - b.z).abs()) as usize
    }
}

impl fmt::Debug for Point {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}, {}, {})", self.x, self.y, self.z)
    }
}

#[derive(Clone, Copy)]
struct Plane {
    point: Point,
    a: f64,
    b: f64,
    c: f64,
    d: f64,
}

impl Plane {
    pub fn from_points(p: &Point, q: &Point, r: &Point) -> Self {
        let v1 = [(q.x - p.x), (q.y - p.y), (q.z - p.z)];
        let v2 = [(r.x - p.x), (r.y - p.y), (r.z - p.z)];

        let [a, b, c] = Self::normalize(&Self::cross_product(&v1, &v2));
        let d = -(p.x * a + p.y * b + p.z * c);

        assert!(0.0 == a * p.x + b * p.y + c * p.z + d);
        assert!(0.0 == a * q.x + b * q.y + c * q.z + d);
        assert!(0.0 == a * r.x + b * r.y + c * r.z + d);

        Self {
            point: Point { x: p.x, y: p.y, z: p.z },
            a: a, b: b, c: c, d: d,
        }
    }

    pub fn distance_to_point(plane: &Plane, point: &Point) -> f64 {
        plane.a * point.x + plane.b * point.y + plane.c * point.z + plane.d
    }

    pub fn distance_to_plane(plane: &Plane, other: &Plane) -> f64 {
        Self::distance_to_point(plane, &other.point)
    }

    pub fn point_intersection(p: &Plane, q: &Plane, r: &Plane) -> Point {
        let m = [[p.a, p.b, p.c],
                 [q.a, q.b, q.c],
                 [r.a, r.b, r.c]];

        let det = Self::det3(&m);
        assert!(0.0 != det);

        let mx = [[p.d, p.b, p.c],
                  [q.d, q.b, q.c],
                  [r.d, r.b, r.c]];
        let my = [[p.a, p.d, p.c],
                  [q.a, q.d, q.c],
                  [r.a, r.d, r.c]];
        let mz = [[p.a, p.b, p.d],
                  [q.a, q.b, q.d],
                  [r.a, r.b, r.d]];

        Point { x: Self::det3(&mx) / det, y: Self::det3(&my) / det, z: Self::det3(&mz) / det }
    }

    pub fn invert(&self) -> Self {
        Self {
            point: self.point.clone(),
            a: self.a * -1.0,
            b: self.b * -1.0,
            c: self.c * -1.0,
            d: self.d * -1.0
        }
    }

    fn cross_product(a: &[f64; 3], b: &[f64; 3]) -> [f64; 3] {
        [
            a[1] * b[2] - a[2] * b[1],
            a[2] * b[0] - a[0] * b[2],
            a[0] * b[1] - a[1] * b[0]
        ]
    }

    fn normalize(a: &[f64; 3]) -> [f64; 3] {
        [a[0] / a[0].abs(), a[1] / a[1].abs(), a[2] / a[2].abs()]
    }

    fn det3(m: &[[f64; 3]; 3]) -> f64 {
        m[0][0] * m[1][1] * m[2][2]
            + m[0][1] * m[1][2] * m[2][0]
            + m[0][2] * m[1][0] * m[2][1]
            - m[0][2] * m[1][1] * m[2][0]
            - m[0][1] * m[1][0] * m[2][2]
            - m[0][0] * m[1][2] * m[2][1]
    }
}

impl fmt::Debug for Plane {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}x + {}y + {}z + {} = 0", self.a, self.b, self.c, self.d)
    }
}

#[derive(Clone, Debug)]
struct Octahedron {
    boundaries: Vec<(Plane, usize)>,
}

impl Octahedron {
    pub fn intersection(a: &Octahedron, b: &Octahedron) -> Option<Octahedron> {
        let mut boundaries:Vec<(Plane, usize)> = vec![];

        for ((plane_a, max_dist_a), (plane_b, max_dist_b)) in a.boundaries.iter().zip(b.boundaries.iter()) {
            let dist_ab = Plane::distance_to_plane(&plane_a, &plane_b);
            let dist_ba = Plane::distance_to_plane(&plane_b, &plane_a);

            if 0.0 <= dist_ab && dist_ab <= *max_dist_a as f64 {
                boundaries.push((*plane_b, usize::min(*max_dist_a - dist_ab as usize, *max_dist_b)))
            } else if 0.0 <= dist_ba && dist_ba <= *max_dist_b as f64 {
                boundaries.push((*plane_a, usize::min(*max_dist_b - dist_ba as usize, *max_dist_a)))
            } else {
                return None
            }
        }

        Some(Octahedron { boundaries: boundaries })
    }

    pub fn corners(&self) -> Vec<Point> {
        vec![
            Plane::point_intersection(&self.boundaries[0].0, &self.boundaries[1].0, &self.boundaries[2].0),
            Plane::point_intersection(&self.boundaries[0].0, &self.boundaries[1].0, &self.boundaries[3].0),
            Plane::point_intersection(&self.boundaries[0].0, &self.boundaries[2].0, &self.boundaries[3].0),
            Plane::point_intersection(&self.boundaries[1].0, &self.boundaries[2].0, &self.boundaries[3].0)
        ]
    }
}

#[derive(Clone, Debug)]
struct Nanobot {
    center: Point,
    r: usize,
}

impl Nanobot {
    pub fn read_line(line: &str) -> Self {
        let parts = line
            .split(['<', ',', '>', '='].as_ref())
            .map(|part| part.parse::<f64>())
            .filter(|result| result.is_ok())
            .map(|result| result.unwrap())
            .collect::<Vec<f64>>();
        Nanobot { center: Point { x: parts[0], y: parts[1], z: parts[2] }, r: parts[3] as usize }
    }

    pub fn contains(&self, point: &Point) -> bool {
        self.r >= Point::d(&self.center, point)
    }

    pub fn as_octahedron(&self) -> Octahedron {
        let xm = Point { x: self.center.x - self.r as f64, y: self.center.y, z: self.center.z };
        let ym = Point { x: self.center.x, y: self.center.y - self.r as f64, z: self.center.z };
        let yp = Point { x: self.center.x, y: self.center.y + self.r as f64, z: self.center.z };
        let zm = Point { x: self.center.x, y: self.center.y, z: self.center.z - self.r as f64 };
        let zp = Point { x: self.center.x, y: self.center.y, z: self.center.z + self.r as f64 };

        Octahedron {
            boundaries: vec![
                (Plane::from_points(&xm, &ym, &zm), self.r * 2),
                (Plane::from_points(&xm, &ym, &zp).invert(), self.r * 2),
                (Plane::from_points(&xm, &yp, &zm).invert(), self.r * 2),
                (Plane::from_points(&xm, &yp, &zp), self.r * 2)
            ]
        }
    }
}

fn read_nanobots(filename: &str) -> Vec<Nanobot> {
    let file = File::open(filename).unwrap();
    let reader = BufReader::new(&file);

    reader.lines().map(|line| {
        Nanobot::read_line(&line.unwrap())
    }).collect()
}

fn part_1(nanobots: &Vec<Nanobot>) -> usize {
    let strongest = nanobots[1..]
        .iter()
        .fold(&nanobots[0], |max, cur| if cur.r > max.r { cur } else { max });

    let mut sum = 0;
    for bot in nanobots.iter() {
        if strongest.contains(&bot.center) { sum += 1 }
    }
    sum
}

fn corner_closes_to_origin(octahedron: &Octahedron) -> Point {
    let origin = Point { x: 0.0, y: 0.0, z: 0.0 };
    let corners = octahedron.corners();

    let mut best_corner = corners[0];
    for corner in corners[1..].iter() { 
        if Point::d(&corner, &origin) < Point::d(&best_corner, &origin) {
            best_corner = *corner;
        }
    }
    best_corner
}

fn part_2(nanobots: &Vec<Nanobot>) -> usize {
    let mut nanobots:Vec<Nanobot> = nanobots.to_vec();
    nanobots.sort_by(|a, b| a.r.partial_cmp(&b.r).unwrap());
    nanobots.reverse();

    let mut intersection = nanobots[0].as_octahedron();
    for bot in nanobots[1..].iter() {
        if let Some(intersect) = Octahedron::intersection(&intersection, &bot.as_octahedron()) {
            intersection = intersect;
        }
    }

    Point::d(&corner_closes_to_origin(&intersection), &Point { x: 0.0, y: 0.0, z: 0.0 })
}

fn main() {
    let input_bots = read_nanobots("input");

    assert!(7 == part_1(&read_nanobots("test1")));
    println!("{}", part_1(&input_bots));
    assert!(36 == part_2(&read_nanobots("test2")));
    println!("{}", part_2(&input_bots));
}
