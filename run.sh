cargo build > /dev/null 2>&1
target/debug/saber test.sbr
node build/load.js
