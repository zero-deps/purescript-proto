"use strict"

exports.fromNumber = num => BigInt(num)
exports.unsafeToNumber = num => Number(num)
exports.fromString = str => BigInt(str)

exports.showImpl = num => num.toString()
exports.eqImpl = x => y => x === y