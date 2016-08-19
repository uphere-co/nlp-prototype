# Haskell script for running bash commends parallely
## Usage
`parsing.hs` : Run Stanford parser and BLLIP parser:
```
#Prepare input files; split the large file.
split -d -l 100000 1b.training.short_sentences.known 1b.training.short_sentences.known.
#Edit&run script!
ghc parsing.hs
./parsing
```

