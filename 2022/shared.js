
// chunks breaks up the array into chunks of the given size.
Array.prototype.chunks = function(size) {
  return (this.length / size).
    times(i => this.slice(i * size, (i+1) * size))
}

// count returns a count of the items that match the predicate
Array.prototype.count = function(pred = null) {
  if(pred === null) {
    return this.length
  }
  return this.filter(pred).length
}

Array.prototype.each = function(fn) {
  for(let idx = 0; idx < this.length; idx++) {
    fn(this[idx], idx)
  }
}

// intersect returns the intersection of both arrays.
Array.prototype.intersect = function(...others) {
  others = others.map(x => x.unique())
  return this.unique().filter(x => others.every(o => o.includes(x)))
}

Array.prototype.last = function() {
  return this[this.length - 1]
}

// max returns the biggest number in this array.
Array.prototype.max = function() {
  return this.reduce((a, b) => a < b ? b : a)
}

// min returns the smallest number in this array.
Array.prototype.min = function() {
  return this.reduce((a, b) => a < b ? a : b)
}

// product returns the product of all values within the array.
Array.prototype.product = function() {
  return this.reduce((a, b) => a * b, 1)
}

Array.prototype.split = function(n) {
  return [this.slice(0, n), this.slice(n)]
}

// sum returns the sum of all values within the array.
Array.prototype.sum = function() {
  return this.reduce((a, b) => a + b, 0)
}

// unique returns an array with each element in `this` once.
Array.prototype.unique = function() {
  const h = this.
        reduce((h, cur) => {
          h[cur] = h[cur] ? h[cur]+1 : 1
          return h
        }, {})
  return Object.keys(h)
}

Array.prototype.cycle = function(n) {
  n = ((n % this.length) + this.length) % this.length
  return this.slice(n).concat(this.slice(0, n))
}

// chars returns an array with the characters of the string.
String.prototype.chars = function() {
  return this.split("")
}

String.prototype.lines = function() {
  const ls = this.split("\n")
  if(ls[ls.length - 1] === "") {
    return ls.slice(0, -1)
  }
  return ls
}

String.prototype.grid = function() {
  return this.lines().map(l => l.split(''))
}

Number.prototype.times = function(fn) {
  const result = []
  for(let i = 0; i < this; i++) {
    result.push(fn(i))
  }
  return result
}

Number.prototype.reduce = function(fn, start) {
  let result = start
  for(let i = 0; i < this; i++) {
    result = fn(result, i)
  }
  return result
}

Object.prototype.map = function(fn) {
  let result = {}
  for(const k of Object.keys(this)) {
    result[k] = fn(k, this[k])
  }
  return result
}

Object.prototype.length = function() {
  return Object.keys(this).length
}
