// chunks breaks up the array into chunks of the given size.
Array.prototype.chunks = function(size) {
  return Array(this.length / size).
    fill(0).
    map((_, i) => this.slice(i * size, (i+1) * size))  
}

// intersect returns the intersection of both arrays.
Array.prototype.intersect = function(...others) {
  others = others.map(x => x.unique())
  return this.unique().filter(x => others.every(o => o.includes(x)))
}

// max returns the biggest number in this array.
Array.prototype.max = function() {
  return Math.max.apply(null, this)
}

// sum returns the sum of all values within the array.
Array.prototype.sum = function() {
  return this.reduce((a, b) => a + b)
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

// chars returns an array with the characters of the string.
String.prototype.chars = function() {
  return this.split("")
}
