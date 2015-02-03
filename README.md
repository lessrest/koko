# Koko, aka `::'

[![Circle CI](https://circleci.com/gh/lessrest/koko.svg?style=svg)](https://circleci.com/gh/lessrest/koko)

```
$ :: [ @print-line Hello, world! ]
Hello, world!
```

```
$ :: [ let hey { [ @print-line hello, %1 ] } : [ @hey world! ] , [ @hey you! ] ]
hello, world!
hello, you!
```

Okay, that doesn't look like much. The syntax is pretty weird. What, yet another language?

Yeah, it's an idea me and my brother came up with on the subway ride home from work.

He's a shell guy. He knows tons of programming languages and stuff, but he really prefers to just use simple shell scripts, as far as possible.

His perfect Platonic idea of a software project is one that consists of a few shell scripts, a `Dockerfile`, maybe a `Makefile` for running various tasks, and maybe a secret file with environment variables.

He knows a lot about bash. I'm pretty sure he could explain all the various `shopt` flags in alphabetical order. You could ask him in his sleep about some obscure shell line and he'd probably tell you how to make it better.

Anyway, since he's pretty familiar with the lexical syntax of bash scripts, he idly suggested that there should be a language that you can type on the shell without any annoying quotes and stuff. Presumably he had been scripting that day, running various `ruby -pe` commands, using `awk` a lot. I think pure shell commands make my brother feel safe.

But I think he's onto something. A lot of people consider the shell to be a pretty obscure environment. You have to read `man` files and it's so annoying to navigate through them for some reason, and the semantics and quoting rules are like a cubist impression of a programming language. Or it's like a haunted house full of dinosaur ghosts from way before the Mac Era.

The graphical user interface is the slightly awkward professional smile of the operating system.

A language is a tool for communication.

A computer language is a tool for communicating with computers.

Computer languages, rather than graphical interfaces, should probably be the primary way to communicate with computers.

I got into Haskell a number of years ago. Me and my brother kind of went separate ways, especially after we moved to different cities. I started to roam toward the astral planes of higher-order functional abstractions. I started seriously dabbling in Agda, a language that mostly is used to prove abstract properties about other abstract programs. In these realms, a program is viewed mostly as a mathematical function with a highly interesting structure. 
I think a lot of people fear the functional realm because they feel it's somehow a bit spacey. They hear about mind-bending stuff like lazy evaluation, recursion, and "monads," and they think it's like science fiction without the lasers. It's like science fiction philosophy. People are talking seriously about stuff like Leibniz's universal algebra, people you know are getting into category theory.

The mathematical structure of computation is really interesting. I didn't get into it much at university, but I remember a few Christmases ago I was talking with my older cousin and I knew she had studied something with computers but not really what specifically and anyway we kind of bonded over lambda calculus for a second.

Turing machines and lambda calculus, I like to think of them as two big trunks growing from the same root. Actually, Leibniz did kind of lay the groundwork for both, if I remember correctly from a presentation I did in school. Man, we got to study stuff like the history of logic. That's such an awesome course. What, logic changed? Yeah, yo. True and false were invented in an [1847 pamphlet](http://www.gutenberg.org/files/36884/36884-pdf.pdf) by a mechanics' institute teacher who quoted Aristotle in the original Greek.

Boolean algebra might be pretty simple, but he started the whole idea of algebraic logic, it seems. Logicians have developed a lot of cool theories beyond the Boolean algebra. There was a quest to find a way to precisely define a method of calculation. These logicians and mathematicians sensed that it would be nice and interesting if we could explain precisely what we mean to calculate. They did not have computers.

Computers would not have existed without these pioneers. Computers, though they are machines, arise from a conceptual structure that grew out of the disciplines of logic, mathematics, and philosophy. Some types nowadays say that philosophy is unnecessary because we have science. Well, inventing formal logic was pretty unnecessary until a few individuals decided to build logical machines.

Can you imagine inventing formal logic without having a computer? I imagine it might as well have happened a thousand years before Christ. It turns out that Pāṇini in the "great ancient city of Pushkalavati," which is now a ruin in Pakistan, invented a formal syntax for Sanskrit in the fourth century B.C. Learning this grammar inspired Ferdinand de Saussure—the "founder of structural linguistics"—and Noam Chomsky, that guy who who devotes his life to documenting the hypocrisy and tragedy of the U.S. ruling class and whose book *Syntactic Structures* meditates on sentences like "colorless green ideas sleep furiously." 

There were the recursion theorists who developed formal theories of recursive relations to explain mathematical and logical reasoning. I feel like this has to start by someone feeling like thinking is too much work. You have to be able to take your math thought, put it down on paper unambiguously, so you can show it to someone else and they'll know exactly what you mean. You would then be able to go on with your life, and other people would be able to use your thought productively in making up new thoughts.

The interesting thing about logic is that its formal study explicitly ignores the contents of thoughts. A specific horse would mostly in logic be considered an abstract value of *P* or *x*. After you bracket away the particulars, you can more easily investigate the structure of a thought, such as an argument or an algorithm. It doesn't study daydreams, it studies thinking that is actively concerned with maintaining some notion of truth.

The structure of an argument and the structure of an algorithm turn out to be closely related. They both involve certain values being accepted as pre-existing arguments and through some non-weird sequence of operations be able to present something new and interesting. To precisely define what "non-weird" means here, logicians and mathematicians started furrowing their brows, thinking, and writing journal articles. It's something like: you can't have new values appear suddenly without clear definition. You need to clearly state your steps in going from one thing to another. Someone else needs to be able to understand what is happening. There is an interesting diversity of ways to combine values giving rise to new ones. It should simply be logical and precise.

Anyway, this is just my brief presentation of stuff I learned in class. And, of course, from reading [*Logicomix*](http://www.nytimes.com/2009/09/27/books/review/Holt-t.html?pagewanted=all), the excellent 2009 comic book about the history of mathematical logic and the quest for certainty. It's about the extremely exciting 20th century chase to find a foundation for mathematics. It was a best seller in Greece. I gave a copy to my friend Alex from the Zen center. He's a young doctor and psychology student and he's always interested in fresh intellectual stuff. Check it out, it's pretty great.

All of this, in a way, leads up to the Curry-Howard isomorphism, which you can read up about yourself on Wikipedia if you don't know about it. Basically, after a lot of parallel developments in the fields of computation and logic, an extremely fruitful discovery was made that joined the fields again. It's also known as the insight that "types are propositions and programs are proofs." Curry was a founder of combinatory logic, who noted that types of combinators could be read as axioms in logical schemes. Howard later discovered that proofs written in a certain style with named values and delimited abstractions corresponded exactly to the lambda calculus. These insights led to very precise formulations about computation and meaning.

This grew into "type theory." For whatever reason, there's a lot of research in Sweden about it, and when I moved to Gothenburg in 2006 to attend university, it was mostly because I knew they did a lot of Haskell. Thierry Coquand was a professor there there who invented the calculus of construction. A lot of people from the Haskell IRC were Gothenburg academics. I'd been there already for a 2005 hackathon where I mostly wrote a Brainfuck parser and marvelled at how smart everybody seemed to be. John Hughes, also a professor there, who invented QuickCheck and arrows and stuff, rode a unicycle in the Chalmers hallway. In Gothenburg, I took [a course with Peter Dybjer](http://www.cse.chalmers.se/edu/year/2013/course/DAT140_Types/) about types for programs and proofs. I stayed up late on weekends reading about the history of operational semantics, and stuff.

I was on the verge of becoming a real language weenie, or type theory grad student. But I slacked off too much. Everything was really interesting, and I felt briefly clever when I implemented some simple group theory proofs from my abstract algebra course as dependently typed Agda programs. I tried to decide if coffee or green tea was the better stimulant for learning to truly appreciate and understand these abstractions. I fell in love with someone who tried to explain [model theory](http://en.wikipedia.org/wiki/Model_theory) to me while having coffee and listening to the violins emanating from the Liszt academy in Budapest, who's now together with a guy researching automatic proof induction. My brain got so tangled up I had to start doing Zen meditation.

This was supposed to be a brief `README`? How much time do I have left? Okay, thanks.

Anyway, I also really appreciated the course on Unix internals, and the operating system stuff, and the parsing, interpreting, and compiling. The more operational stuff. I've always been interested in being able to build programs that do stuff. And I loved the Unix idea of having a command line interface that's also a programming language optimized for brevity and power.

Steve Jobs ceremoniously married Unix to the graphical user interface. Great idea, guy!

A lot of programmers I know have Macs because for three reasons. Their hardware works mostly well with their operating system, especially concerning day-to-day stuff like plugging in various gadgets, hibernating, and connecting to Wi-Fi networks. They have a bit of aesthetical sensibility. And they run Unix.

Neal Stephenson wrote an essay called ["In the Beginning was the Command Line"](http://cristal.inria.fr/~weis/info/commandline.html). Here is a quote; he's describing the world of Linux circa 1999:

> It's a bunch of RVs, yurts, tepees, and geodesic domes set up in a field and organized by consensus. The people who live there are making tanks. These are not old-fashioned, cast-iron Soviet tanks; these are more like the M1 tanks of the U.S. Army, made of space-age materials and jammed with sophisticated technology from one end to the other. But they are better than Army tanks. They've been modified in such a way that they never, ever break down, are light and maneuverable enough to use on ordinary streets, and use no more fuel than a subcompact car. These tanks are being cranked out, on the spot, at a terrific pace, and a vast number of them are lined up along the edge of the road with keys in the ignition. Anyone who wants can simply climb into one and drive it away for free.

And on the topic of command lines and graphical user interfaces:

> People who have only interacted with computers through graphical user interfaces like the MacOS or Windows--which is to say, almost everyone who has ever used a computer--may have been startled, or at least bemused, to hear about the telegraph machine that I used to communicate with a computer in 1973. But there was, and is, a good reason for using this particular kind of technology. Human beings have various ways of communicating to each other, such as music, art, dance, and facial expressions, but some of these are more amenable than others to being expressed as strings of symbols. Written language is the easiest of all, because, of course, it consists of strings of symbols to begin with. If the symbols happen to belong to a phonetic alphabet (as opposed to, say, ideograms), converting them into bits is a trivial procedure, and one that was nailed, technologically, in the early nineteenth century, with the introduction of Morse code and other forms of telegraphy.

The advance of the graphical user interface:

> It was clear the the Mac's engineers saw a whole new country stretching out before them; you could almost hear them muttering, "Wow! We don't have to be bound by files as linear streams of bytes anymore, vive la revolution, let's see how far we can take this!" No command line interface was available on the Macintosh; you talked to it with the mouse, or not at all. This was a statement of sorts, a credential of revolutionary purity. It seemed that the designers of the Mac intended to sweep Command Line Interfaces into the dustbin of history.

He points out how Unix is the only remaining user operating system that's entirely separate from a graphical user interface—that is fully functional with only a command line.

On the graphical user interface as a mediated experience:

> Americans' preference for mediated experiences is obvious enough, and I'm not going to keep pounding it into the ground. I'm not even going to make snotty comments about it—after all, I was at Disney World as a paying customer. But it clearly relates to the colossal success of GUIs and so I have to talk about it some. Disney does mediated experiences better than anyone. If they understood what OSes are, and why people use them, they could crush Microsoft in a year or two.

That's a pretty interesting thought. Marshall McLuhan has a lot of these interesting ideas about what businesses are really in the business of doing—like how General Electric, as a light bulb company, was actually in the information business, which if taken more seriously had maybe led to interesting things. Maybe Microsoft could crush Disney. Now they have Minecraft. People like to build cool structures out of blocks. Even kids.

You just have to make it kinda fun and not horrible.

Ruby is pretty fun. But it's such a... programming language.

I'm a little tired of programming.

Sure, I'll get my hands dirty and make a new program once in a while, as suits the occasion. But to always be writing more and more code into some big program, it just seems a little much.

For a while I've thought about myself more as someone who configures machine software.

I don't make the machines. I don't write the operating systems. I don't write programs of algorithmic or mathematical novelty or importance. I mostly avoid writing web servers and browser engines. What I do, mostly, is configure other software.

Some configuration languages I use not straightforwardly "Turing complete," that is, they can maybe only through very strange tricks be used to perform general computation. Instead, for example, HTML and CSS configure web display engines in rather sophisticated ways. A `Dockerfile` specifies how to create a isolated virtual machine that recreates an exactly specified Unix environment. A `sed` script performs automatic editing on streams of text. And so on.

Sometimes you need to venture into the difficult and error-prone realm of general purpose programming. But mostly, I just want to hook things together, do a bit of glueing and duct-taping, and then sell my startup for a billion dollars.

This is my dream. To make programming in the Unix environment slightly more fun.

There is a tradition of weird and fun languages in Unix.

Regular expressions might have been one of the first ones—also originating in mathematical logic, interestingly, since they're now seen as somewhat kludgy. They had regexps in `ed`, `sed`, `vi`, `emacs`, `awk`, `perl`, `tcl`, `ruby`. Concise and declarative pattern matching is fun! 

What else can we make concise and fun?

Serving HTTP? Maybe something like:

```:: @using http { @serve { GET / { @respond @read-file index.html } } }```

Working on queue tasks?

```REDIS_URL=db.example.com :: @using redis { @forever { @echo @BRPOP tasks } }```

I just want to make the computer do things.
