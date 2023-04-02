"use strict"

export const length = xs => xs.length

export const indexUnsafe = xs => i => xs[i]

export const slice = xs => start => end => xs.slice(start, end)

export const newUint8Array = len => new Uint8Array(len)

export const fromArray = xs => new Uint8Array(xs)

export const fromArrayBuffer = xs => new Uint8Array(xs)

export const concatAll = xs => {
  const zs = new Uint8Array(xs.reduce((acc, x) => acc + x.length, 0))
  xs.reduce((acc, x) => {
    zs.set(x, acc)
    return acc + x.length
  }, 0)
  return zs
}

export const eqImpl = xs => ys => (xs === ys) || (xs.length === ys.length && xs.every((x,i) => x === ys[i]))
