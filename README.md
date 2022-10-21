# notRedis
I decided to challenge myself and write this in scala, which I've never used before. I used scala 3 but the only scala 3 feature I used was union types as far as I know.

I implemented SET, GET, LPUSH, LPOP, LRANGE, HSET, and HGET

Things to note:

- in LRANGE: negative indexes indicated offsets starting at the end. So -1 refers to the last index, -2 is the second to last, etc.
- spaces can be included by surrounding them with double quotes (example: "argument with spaces").
- double quotes can be included by prefixing them with a backslash (example: argumentWithAQuote\"InIt).
- should be thread-safe but I haven't tested this. It's also not completely optimized thread-safety becayse I just used a simgle read-write lock when it would probably be better to lock on a per-entry basis.
