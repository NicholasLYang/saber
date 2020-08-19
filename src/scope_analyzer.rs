// Takes functions inside scopes and gives them unique names. That way functions can be stored
// in a flat table

// let a = \() => {
//    let b = \() => {
//
//    }
// }
// let b = \() => {
//
// }

// This becomes

// let a0 = \() => {
//    let b1 = \() => {
//
//    }
// }
// let b0 = \() => {
//
// }
