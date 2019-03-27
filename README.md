# Haskell Solitaire

This is a final project for a functional languages class. It's a simple 1-card
draw solitaire game played from the command line. The PlayingCard library could
easily be extended and adapted for other card games written in Haskell.

## Gameplay

The board prints using special characters to create card shapes and escape sfor
full functionalityequences to color the cards. Here's what a new game looks
like:

```text
           W                   F1        F2        F3        F4
┌┌─────┐  ┌─────┐             ┌─────┐   ┌─────┐   ┌─────┐   ┌─────┐
││ ≡≡≡ │  │     │             │     │   │     │   │     │   │     │
││ ≡≡≡ │  │     │             │     │   │     │   │     │   │     │
││ ≡≡≡ │  │     │             │     │   │     │   │     │   │     │
└┕─────┘  └─────┘             └─────┘   └─────┘   └─────┘   └─────┘
 T1        T2        T3        T4        T5        T6        T7
┌─────┐   ┌─────┐   ┌─────┐   ┌─────┐   ┌─────┐   ┌─────┐   ┌─────┐
│J   ♣│   │ ≡≡≡ │   │ ≡≡≡ │   │ ≡≡≡ │   │ ≡≡≡ │   │ ≡≡≡ │   │ ≡≡≡ │
│     │   ┌─────┐   ┌─────┐   ┌─────┐   ┌─────┐   ┌─────┐   ┌─────┐
│♣   J│   │3   ♠│   │ ≡≡≡ │   │ ≡≡≡ │   │ ≡≡≡ │   │ ≡≡≡ │   │ ≡≡≡ │
┕─────┘   │     │   ┌─────┐   ┌─────┐   ┌─────┐   ┌─────┐   ┌─────┐
          │♠   3│   │8   ♣│   │ ≡≡≡ │   │ ≡≡≡ │   │ ≡≡≡ │   │ ≡≡≡ │
          ┕─────┘   │     │   ┌─────┐   ┌─────┐   ┌─────┐   ┌─────┐
                    │♣   8│   │6  ♡ │   │ ≡≡≡ │   │ ≡≡≡ │   │ ≡≡≡ │
                    ┕─────┘   │     │   ┌─────┐   ┌─────┐   ┌─────┐
                              │♡   6│   │A   ♣│   │ ≡≡≡ │   │ ≡≡≡ │
                              ┕─────┘   │     │   ┌─────┐   ┌─────┐
                                        │♣   A│   │3   ♣│   │ ≡≡≡ │
                                        ┕─────┘   │     │   ┌─────┐
                                                  │♣   3│   │2  ♡ │
                                                  ┕─────┘   │     │
                                                            │♡   2│
                                                            ┕─────┘

Let's play a game of Solitaire!
Type 'h' for help.
Command:
```

And here is the help text:
```text
Terminology:
  The Tableau: Seven piles that make up the main table. (labeled T1 - T7)
  The Foundations: Four piles on which a whole suit must be built up in sequence
                   from Ace to King. (labeled F1 - F4)
  The Stock (or "Hand") Pile: The main deck containing the remaining cards to
                              draw from.
  The Talon (or "Waste") Pile: Cards drawn from the Stock that have no place in
                               the Tableau. (labeled W)

Command options:
  d : Draw a card from the Stock
  m : Move a card. Requires 2-3 parameters: n, from, and to.
      Parameters are case-insensitive so T1 == t1.
      e.g. "m 2 T1 T2" : Move 2 cards from Tableau 1 to Tableau 2
           "m T2 F1"   : Move all valid cards from Tableau 1 to Foundation 1

      n    : Optional: The number of cards to move. Can only move cards that are
             face up. If not provided, the maximum number of cards will be
             moved.
      from : Where to move card(s) from.
             W     : The Talon (or "Waste") Pile
             T1-T7 : The Tableau
             F1-F4 : The Foundations
      to   : Where to move card(s) to.
             T1-T7 : The Tableau
             F1-F4 : The Foundations
  a : Auto move cards. Attempts to move all available cards
      in ordered sequence to the Foundation piles.
```

## Installing

Clone/Download the source code:
```
git clone git@github.com:lukexor/haskell_solitaire.git
```

Ensure System.Random is installed:
```
cabal install random
```

Compile binary executable:
```
ghc solitaire.lhs -o solitaire -main-is Solitaire
```

Start a new game:
```
./solitaire
```

## Prerequisites

[System.Random](http://hackage.haskell.org/package/random-1.1/docs/System-Random.html)

## Environment

This implementation was designed on Mac OS Mojave using iTerm2 and was not
tested on other platforms.

## Built With

[GHC](https://www.haskell.org/ghc/)

## Authors

* **Lucas Petherbridge** - *Design/Implementation* - [lukexor](https://github.com/lukexor/)

## License

This project is licensed under the GNU GENERAL PUBLIC License - See the
[LICENSE](LICENSE) file for details.
