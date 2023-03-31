"use strict"

export const fromNumber = num => BigInt(num)
export const unsafeToNumber = num => Number(num)
export const fromString = str => BigInt(str)

export const showImpl = num => num.toString()
export const eqImpl = x => y => x === y