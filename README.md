# The Biz programming language
## Welcome to Biz
Biz is a bizarre programming language based on [JoJo's Bizarre Adventure](https://en.wikipedia.org/wiki/JoJo%27s_Bizarre_Adventure). I followed the guide [Write Yourself a Scheme in 48 Hours](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours) as a starting point. It's interpreted and written in Haskell.

## Contributing
Do you have a suggestion for a new construct? Do you think something should be changed? Feel free to open an issue.

## Installing
Build with stack and run like this.
```
stack run "filename.bz"
stack run
```
If you don't specify a file, the REPL will be launched. It isn't ready (like the language), but it could be hepful.

## Language overview

### Binding a value to a name
Sometimes, it's useful to keep track of important values or giving them a more interensting name. How can we do it? With the `kono <name> <value> da` expression.

`kono diego 4 da` means: binds the value `4` to the name `diego` and give back the value `4`. From now on, we can refer to the name `diego` instead of using directly the value `4`. We can update the value bond to the name `diego` in the same way: `kono diego 6 da`.

Consider the following code

	kono a 1 da
	kono b a da
	kono b 2 da
	
When we use the `kono...da` expression, we make a copy of the value. Consequently, `a` is still `1`, while `b` is updated to `2`. The type of the value isn't important. We can update the value bond to a name for different types:

	kono iggy 1 da
	kono iggy "coffee" da
	
This means: bind the value `1` to the name `iggy` and update it with the value `"coffee"`. From now on, we can use the name `iggy` instead of using the value `"coffee"` directly. It isn't important if we update the value bound to the name `iggy` with a value of a different type.

A name can be `reliable`, that is, its value can't be updated. Trying to update its value will give you an error

	kono myVar yesyesyes da
	kono reliable myVar da
	kono myVar 4 da speedwagon error

### Comments
The purpose of comments is to add notes about your code or to avoid executing the beautiful code you have just written. Everything in the comments is ignored. A comment begins with the word `speedwagon` and ends with a newline

	speedwagon He's Immortal! Unkillable! Unmatched!
	kono Kars "ultimate lifeform" da

Why having multi-line comments when we can have multiple single-line comments?

	speedwagon This is the first comment line
	speedwagon and this is the second one

### Logical values
Some programming languages have things called boolean or the concept of "truthiness" for things that are true or false, black or white, 0 or 1. Biz has two expressions that accomplish this purpose: `yes` and `no`. They can be repeated more than one time: `yesyesyes`, `nono`, `yesyesyesyesyes` are valid expressions. 

	kono bool yes da
	speedwagon bool holds the yes value
	
	kono bool nonono da
	speedwagon bool holds the no value

	kono bool boingo yesyes and no
	speedwagon bool hols no, the result of yesyes ∧ no
| Action        | Result                                               |
| ------------- | -------------                                      |
| `and`         | `yes` if both values are `yes`, `no` otherwise       |
| `or`          | `yes` if at least one value is `yes`, `no` otherwise |
| `xand`        | `yes` if both values are the same, `no` otherwise       |
| `xor`         | `yes` if only one value is `yes`, `no` otherwise     |
| `equal`           | `yes` if the values are the same, `no` otherwise     |

There is only one action that takes only one Both value. It's `opposite` and it gives `yes` if the value is `no`, `no` otherwise.

### Pointless and Pointplus number
Numbers are cool and they let us make amazing things. In Biz we can express numbers in two ways: with a point (pointless) or without a point (pointplus). Examples of pointless numbers are `0`, `1`, `-1`, `4`. On the other hand, pointplus numbers could be the following: `0.1`, `-4.4`, `123.444`. You can manipulate pointless numbers and pointplus numbers with the following primordial binary actions (you can't mix pointless and pointplus numbers):

| Action        | What it gives                                                                     |
| ------------- | -----                                                                             |
| `+`           | the sum                                                                           |
| `-`           | the difference                                                                    |
| `*`           | the product                                                                       |
| `/`           | the quotient                                                                      |
| `=`           | `yes` if the numbers are equal, `no` otherwise                                    |
| `/=`          | `yes` if the numbers are not equal, `no` otherwise                                |
| `>`           | `yes` if the first number is greater than the second one, `no` otherwise          |
| `<`           | `yes` if the first number is less than the second one, `no` otherwise             |
| `>=`          | `yes` if the first number is greater or equal than the second one, `no` otherwise |
| `<=`          | `yes` if the first number is less or equal than the second one, `no` otherwise    |
| `\`           | the remainder (only for pointless numbers)                                        |
|               |                                                                                   |

### Text
You can store text by enclosing it with the symbol `"`. The character `\` is used to insert characters hard to type: `\n`, `\t`, `\r`, `\\` , `\"`. Emoji are supported. `"I am Yoshikage Kira..."`, `"a"`, `""`, `"\\\r\\t"`, `"🗿"`, are valid Text values.

| Action        | What it gives     |
| ------------- | -----            |
| `++`          | adds to the end   |
| `--`          | adds to the start | 
|               |                   |

### Sequence/Chain`{...}`
It's possible to chain multiple actions by enclosing them between `{` and `}` and separating them with a newline. The sequence expression is useful when you want to evaluate multiple expresssions (e.g. the body of an action)

	{ 
	  yes
	  123
	  no
	}

The result of the evaluation of a sequence expression is the last expression in the list. The previous code evaluates to `no`. The empty sequence expression (`{}`) gives the value `none`

### Will you do the oraora thing?
This expression let you do something based on a condition

	will i hit you with my right fist or my left <condition>
	right <yes_expression>
	left <no_expression>
	both <always_expression>
	
`<condition>` is the condition to be evaluated. If it is evaluated to `yes`, `<yes_expression>` will be evaluated. Otherwise, `<no_expression>` will be evaluated. `<always_expression>` will be always evaluated. `left`, `right` and `both` are optional. If none of them is provided, the whole expression will be evaluated to `none`. It is possible to use `which fist` instead of `will i hit you with my right fist or my left`.

	which fist yes

is a valid expression.


### Actions
Actions are blocks of code that can take some input values (parameters) and can give a result. The action in the example below is called `multiply`, it takes two parameters (that should be numbers) and it multipies them, giving us the product:

	oingo multiply x y :
	   boingo x * y

`oingo` is used to create an action, while `boingo` is used to call an action.

If you want to summon an action, the expression `boingo <action> (<params>)`. `<action>` is the name of the action we want to call (e.g. `+`, `and`, etc.), `<params>` is a comma separated list of parameters you want to feed to the action.

	oingo + 1 2 :
	oingo and yes no
	
In case of a binary action (an action that takes exactly two parameters), we can call it with the syntax `boingo <param1> <action> <param2>`.

	oingo 1 + 2
	oingo yes and no

We can't update the value bond to a name outside the action within the action. Consider the following example:
	
	kono name "value" da
	boingo action : : kono name "new value" da
	speedwagon error

In order to be able to update it, we must add the attribute `no dignity` (or `nodignity`) to the name.

	kono no dignity name "value" da
	boingo action : kono name "new value" da
	speedwagon "new value" is bound to name

It is possible creating an action and immediately calling it

	oingo boingo greet name : oingo echoes name : "hol horse" :

Let's split it:
- `boingo greet name : oingo echoes name :` creates and gives an action called `greet` that takes one parameter called `name`. Its body consists of calling the action `echoes` with the `name` as its argument.
- `oingo ... "hol horse" :` calls the action just created with `"hol horse" as its argument.

If we don't want to create the action but just using it one time, we can replace the action name with `combo`

	oingo boingo combo name : oingo echoes name jo "hol horse" jo
	
### Oh Doppio, watashi no kawaii Doppio...
An handy construct for storing values is the `doppio` one. It is possible to store any amount of values of any type.

	speedwagon the empty doppio
	doppio
	speedwagon a doppio with three elements
	dop 1 "bossu" nonono pio

The `dururu` construct let us take the `ru`th element from the start. In other words, with `duru` we take the first item, with `dururu` the second one, and so on. `rurudu` works in the same way, but it takes the `ru`th element from the end.

	speedwagon takes the 4th element from the start (the second "a")
	dururururu dop "a" "b" "b" "a" "c" "c" "h" "i" "o" pio
	
	speedwagon takes the 4th element from the end (the second "c")
	rurururudu dop "a" "b" "b" "a" "c" "c" "h" "i" "o" pio
	
Another important construct is `moshimoshi`. Used with `dururu` or `rurudu`, updates that particular element with a new value. It doesn't affect the original `doppio`.

	kono gio dop "g" "i" "o" "v" "a" "n" "n" "a" pio da
	dururururururururu moshimoshi "i" gio
	speedwagon gio is dop "g" "i" "o" "v" "a" "n" "n" "i" pio

### Sequence of expressions
As our programs become bigger and complicated, we need to evaluate multiple expressions. This is the purpose of the sequence construct. Check the following example:

	{}
	
This is the empty sequence and it's evaluated to `none`, a special value that can't be used. Let's see a more useless example:

	{
	   kono unluckyNumber 4 da
	   oingo unluckyNumber * unluckyNumber
	}

First, we introduce a name for the value `4`, then, we multiply it for itself. The result of the expression is `16`,  the result of the last expression in the sequence. In more complex sequences, you often don't want to evaluate every single expression. Consider this snippet:

	boingo fact number : {

       will i hit you with my right fist or my left oingo = number 0 jo
       right 1
       left oingo * number oingo fact oingo - number 1 jojojo

	}

This is the Biz rapresentation of the well-know [factorial](https://en.wikipedia.org/wiki/Factorial) function. Everything works well for positive numbers, but what happens with negative numbers? The recursion doesn't have a base case and we get stuck. We can exit immediatly if the number provided is negative in order to the make the action a little more safe.

	boingo aLittleMoreSafeFact number : {

       which fist oingo < number 0 :
       right arrivederci -1
 
       will i hit you with my right fist or my left oingo = number 0 jo
       right 1
       left oingo * number oingo fact oingo - number 1 jojojo

	}
The action `aLittleMoreSafeFact` works like `fact`, but we get the value `-1` when we call it with a negative number. When we encounter  `arrivederci`, its expression is evaluated and its result is the result of the whole sequence.Consequently, the expressions after `arrivederci` aren't evaluated.

There is another way to write the `arrivederci` expression:
	
	[ari]*arri <expression> vederci
	
Any amount of `ari`, followed by `arri`, the expression and `vederci`

	boingo goodbye name :
	   ariariariariarri oingo ++ "Arrivederci " name : vederci
	speedwagon "Arrivederci Pesci"

### Looping
One of the most common things in programming is doing the same thing over and over. Do you know how Gold Experience Requiem works? There are 3 kinds of loops.

#### Infinite loop
The infinite loop doesn't have an exit condition and it's written in the form `goldexperiencerequiem <expression>` (or just`ger <expression>`).

	`goldexperiencerequiem` oingo echoes "wha-" jo

This line of code keeps printing the text `"wha-"` indefinitely. what if we want to exit the loop after just one iteration?

	ger {
	   oingo echoes "wha-" jo
	   arrivederci
	}
	speedwagon prints "wha-"

In other words, we have a sequence of expressions (delimited by the characters `{` and `}`) which contains an action call and the `arrivederci` expression.

#### Range loop
The range loop has the form `gold <localName> <startingValue> experience <increment> requiem <condition> <body>`.  Check the following example:

	gold sexPistol 1 experience 1 requiem oingo < sexPistol 8 jo
	   oingo echoes oingo ++ "Sex Pistol number " sexPistol jojo 
	   speedwagon prints "Sex Pistol number 1"...

The starting value is `1` and it can be retrieved with the `sexPistolName`;`1` is the increment, that is, the value it will be added to the starting value; `oingo < sexPistol 8 jo` is the keep-looping codition: if it's `yes` we keep looping, if not, we exit the loop; finally we have the body which prints a line. Going back to our example, Sex Pistol number 4 doesn't exist, so we can avoid printing it:

	gold sexPistol 1 experience 1 requiem oingo < sexPistol 8 jo {
	    which fist oingo = sexPistol 4 jo
	    right king crimson
	    oingo echoes oingo ++ "Sex Pistol number " sexPistol jojo
	}

`king crimson` (or `emperor crimson`) skips the current loop iteration, forcing the evaluation to the next one.

#### Doppio loop
Range loop are handy, but not so handy while working with `doppio`. Let's try to recreate the previous example using `doppio` to store all the Sex Pistols values:

	kono sexPistols dop 1 2 3 5 6 7 pio da
	gold sexPistol 1 experience 1 requiem oingo < sexPistol 7 jo {
	    oingo echoes oingo ++ "Sex Pistol number " duru sexPistols jojo
	    kono sexPistols oingo beep sexPistols jo da     
	}

We fetch the first element of `sexPistols` and we print it. Then, we create an new `doppio` without the first element (`beep` action) and we bind it to to `sexPistols`. Let's see the same code expressed with the doppio loop:

	ger sexPistol dop 1 2 3 5 6 7 pio
	    oingo echoes oingo ++ "Sex Pistol number " sexPistol jojo

### Users, stands, abilities and arrows
`doppio` is useful when you want to store values in a specific order. Stands are used to store values without any order and with a specifi name. Let's see how everything works:

	speedwagon we create a new user called Jotaro
	user Jotaro
	
	speedwagon we create a new stand called StarPlatinum
	stand StarPlatinum
	
	speedwagon now Jotaro has the StarPlatinum stand
	(StarPlatinum -> Jotaro)
	
	speedwagon we create a new stand called TheWorld and we give it two abilities
	stand TheWorld : ability stopTime 1; ability mudamuda "oh-oh" :
	
	speedwagon we give TheWorld to Jotaro
	(TheWorld -> Jotaro)
	
	speedwagon we give an ability to Jotaro's StarPlatinum
	speedwagon StarPlatinum itself isn't updated
	((ability starFinger "oraoraora") -> StarPlatinum -> Jotaro)

We create users, we create stands, we give stands some abilities and we give users some stands using the `->` (arrow) operator. There is `<-` (backward arrow) operator: it works in the same way, but in the opposite direction:
	
	(user Okuyasu) <- (stand ZaHando) <- (ability deleteSpace 1)

We can retrieve stands and abilities using the `!` (cry) operator:

	Okuyasu!!!ZaHando!deleteSpace!!!!!

There is no limit to the amount of `!`
	
### Moody Blues
The power of `moodyblues` is to repeat things already done. Let's see how it works:

	{
	   oingo echoes "first" jo
	   oingo echoes "second" jo
	   oingo echoes "third" jo
	   moody blues 2
	   oingo echoes "fourth" jo
	}

This code will print:

	"first"
	"second"
	"third"
	"second"
	"third"

`moody blues <number>` moves the execution back of `<number>` expressions and it will get ignored the next time it will be encoutered. Don't worry if `<number>` is greater then the number of avaible expressions. The execution will wrap around from the end of the sequence

	﻿{
	   oingo echoes "first" jo
	   oingo echoes "second" jo
	   oingo echoes "third" jo
	   moody blues 9
	   oingo echoes "fourth" jo
	}

The output is:

	"first"
	"second"
	"third"
	"third"
	"fourth"

### Killer Queen, daisan no bakudan, bites za dusto!	
For a better learning experience, listen to [this](https://www.youtube.com/watch?v=svE60ic4akE).

During the execution of a Biz program, we bind many values ​​with as many names. Killer queen introduces a way to have multiple snapshots of the bindings. An example will make it clear.

	kono hayato "perv kid" da
	killer queen hand

The second line creates a snapshot of all the current bindings, `hayato` included. Now, if we update `hayato` with a new value or we introduce new bindings, the bindings in the snapshot aren't updated.

	kono another 3 da
	bites the dust hand

The first line creates a new binding, but the second line loads the snapshot called `hand` overrinding all the current bindings, `another` included. Trying to get value bond to `another` wil lresult in an error. It is possible having multiple snapshots at the same time:

	kono hayato "smart kid" da
	killer queen strayCat
	kono coolCat "nice song" da
	killer queen hotSpace
	bites za dusto strayCat
	
### Mista (WIP)
The `mista` contruct is used to check if an expressions evaluates to something related to the number four

### Part skippers
Splitting code in multiple files is the way to go if you want to become a successful Biz programmer. This is accomplished with parts. Every part must be defined in a file with the same name. For example, the part `StardustCrusaders` is defined in a file named `StardustCrusaders.bz`. It is possible to put parts in subdirectories. The part `Stardust.Crusaders` must be in a directory called `Stardust` which has a file named `Crusaders.bz`. The first expression of each file must be `part <part_name>`. For example:

	part Narancia

This line says: "from now on, everything belongs to the `Narancia` part. That is, you can call bindings specifying the part they belong to (it isn't mandatory):

	kono firstFactor 16 da
	kono secondFactor 55 da
	kono product oingo * Narancia.firstFactor Narancia.secondFactor jo da
	speedwagon product isn't 28

We can bring other parts to our part. Let's say we have another part called `Stardust.Crusaders`

	part Stardust.Crusaders
	
	kono silverTwist "nice song!" da
	kono product 28 da

We can add it to our part like this:

	part Narancia
	skip part Stardust.Crusaders

	oingo echoes silverTwist jo
	oingo echoes StardustCrusaders.product jo
	speedwagon prints 28

We aren't forced to use the namepart. We can skip the part with a new name:

	part Narancia
	skip part Stardust.Crusaders as ShitpostCrusaders

	oingo echoes silverTwist jo
	oingo echoes ShitpostCrusaders.product jo
	speedwagon prints 28

## Things to add and improve
- mista
- so it's the same stand as ...
- this is a test
- repl
