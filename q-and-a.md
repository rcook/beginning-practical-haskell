# Questions and answers

This will eventually become Frequently Asked Questions once the questions have been asked more than once each!

## Q. If a thunk is forced from more than one thread, are there any guarantees that a value will not be evaluated more than once?

My hand-wavy answer to this was "It doesn't matter" since referential transparency guarantees that whether the inner computation was performed more than once cannot be observed by outside users. But, of course, this is not really true since more than one computation could incur a time or performance overhead that could be observed by the end user.

My claim is that Haskell programming language specification does not go into this and an implementation is free to deal with this however it sees fit.

I will look into the details of this and get back to you when I have a more satisfactory answer.