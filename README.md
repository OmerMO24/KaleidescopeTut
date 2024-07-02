Hello Everyone who may or may not see this I am not sure if I am going to make this repo public or not.

This is a little project called kaleidescope that implements a simple language called, as you guessed it, Kaledeiscope.
Kaleidescope uses LLVM to generate code. It basically takes your code -> generates an intermediate representation -> then takes it down to assembly (need to do some more research on LLVM lowering so I will make this more detailed as I learn more).

LLVM have been very generous to include a lot of documentation so following this tutorial should be straightforward.

Why am I doing this you may ask?

Well, towards the latter part of my second semester of university I lowkey fell in love with operating systems. It's the first time I really felt like we touched on the "computer" 
part in "computer science".
I've always wanted to make things go fast, and realized that the easiest way to achieve that is to exploit the hardware that our programs run on.
And programs are written in coding languages, and some languages are faster than others, and I wanted to know why. 
Narrowing my focus down to so called "fast languages", we have our usual culprits like C and C++, and some new kids on the block in the form of rust, GO, and Mojo.
I discovered the existence of LLVM, and realized that creating a programming language of my own may not be as daunting/impossible of a task as I previously thought.
So I came up with the idea of TERSE. 

TERSE is a conceptual language that I hope to start working on after finishing kaleidescope.
TERSE is, well, terse. Im aiming for a low level, "0 cost abstraction" language, that can be really fast but also not frighteningly esoteric. I want it to be accessible. 
TERSE will do away with OOP completely (I don't really feel like I need to explain why OOP is bad in every sense of the word). However we will have structs and basic data structures.

For now, TERSE is aimed to be a language that is kinda fast (I'm aiming for at least as fast as C++ and rust, mojo will follow). 

That's enough about TERSE, but, more will folllow as I learn more about llvm infrastructure and what is possible. 

But yeah,for now the entirety of the kaleidescope implementation will be in one file, but, Once I know it's working, i'll split the lexer, parsers, and code-gen into separate files.

I'll add my website link sometime soon when I get it up and running, I'll be discussing some cool OS/hardware/language concepts, so check that out once it's on here.

signed, 0x776982.


# Update on 2/7/2024
So as usual, things don't always go your way. After going through the chapter for JIT'ing the IR, I kept running into segFaults that I couldn't seem to fix. LLVM's documentation is weird in that the code that they show in their explanations does not match the official code listing shown at the end of every chapter. To maximize my time learning, i've already taken notes on all the new implementation done in the JIT chapter, and will continue working on the code listing from LLVM's website rather than my own (All of the code up to the JIT chapter is literally the exact same, just reorganized in a cleaner more concise way, so its like Im using my version). However, I will take some time out to figure out where I messed up in my own copy, which should just be an aboslute blast don't we love seg faults, im jk major skill issue from me. However, we push on. 
