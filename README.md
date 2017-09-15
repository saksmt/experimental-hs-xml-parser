# xml-parser

Experiments in Haskell: simple XML parser

## Usage

### Build

```bash
$ stack build
```

### Execution

```haskell
import Xml
import Text.Parsec

myParsedXml = parse xmlP "xml" $ mconcat
    [ "<?xml charset=\"utf-8\"?>"
    , "<someTag>"
    ,   "<tagWithText and=\"attributes\">"
    ,       "some text"
    ,   "</tagWithText>"
    ,   "<selfClosingTags with=\"attributes\" />"
    , "</someTag>"
    ]
```

