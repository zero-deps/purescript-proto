"use strict"

exports.length = xs => xs.length

exports.indexUnsafe = xs => i => xs[i]

exports.slice = xs => start => end => xs.slice(start, end)

exports.fromArray = xs => new Uint8Array(xs)

exports.concatAll = xs => {
  const zs = new Uint8Array(xs.reduce((acc, x) => acc + x.length, 0))
  xs.reduce((acc, x) => {
    zs.set(x, acc)
    return acc + x.length
  }, 0)
  return zs
}

exports.eqImpl = xs => ys => (xs === ys) || (xs.length === ys.length && xs.every((x,i) => x === ys[i]))
