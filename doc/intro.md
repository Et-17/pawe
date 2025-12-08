# Documentation

Pawe does its best to be highly versatile and applicable to many different types of linguistics, but that means there's a lot of switches to set in configuring it. I've done my best to follow modern linguistics approaches but if something's wrong or awkward, please feel free to open an issue or submit a pull request.

## Attributes

Everything in Pawe is based around the "attribute", roughly corresponding to the concept of distinguishing features. Attributes can be a "feature", which is a switch that is either positive, negative, or unset. They can also be a "parameter" which is a set of options, called "variants". Phonemes that set a parameter are always only one of the variants, but filters and selectors can have either have a positive parameter, which matches phonemes with a specific variant, or a negative parameter, which matches phonemes that are not a specific variant. We will talk more about that in the section on filters and selectors.

Features can be defined using a `features` block. You can use as many of these as you want and place them wherever you want. All features are useable everywhere once they've been defined, but before that point in the file, the feature doesn't "exist". For example, if you were about to start using consonants and wanted the features `voiced`, `labialized`, and `lateral`, you could write
    
    features {
        voiced;
        labialized;
        lateral;
    }

Now those three features can be used anywhere in the rest of the configuration file. Anything created before a feature was defined is simply considered as unset for that feature. Features are written as `+feature` or `-feature`.

Parameters can be defined using a `parameters` block. Just like features, you can use as many of these as you want and place them wherever you want. For example, if you were about to start using vowels, and you need to denote whether a vowel is close, mid, or open, and whether it's front, central, or back, then you could write

    parameters {
        height {close; mid; open}
        backness {front; central; back}
    }

Parameters are written as `+parameter.variant` or `-parameter.variant`. If you use a parameter without specifying a variant, it will be parsed as a feature and deliver an error if there is not a feature by that name. Even though phonemes can't have negative parameters, you still have to specify the parameter in the positive; if you leave it unmarked, it will be parsed as a character.

## Atoms

Attributes are assembled into "atoms", which refers to anything that broadly represents a phoneme in the target word. There are three types of atoms: phonemes, filters, and selectors.

### Phonemes

A phoneme is a definite collection of attributes that, in theory, represents a phoneme in the word. They are defined using a space separated list of attributes inside of `[...]`. Characters and selected phonemes (see the section on selectors) are processed as if they were a list of their attributes. If an attribute that was already set is defined again, the new definition will silently replace the old one. This is useful for modifying a character or selected phoneme. For example, if you had a character `t` representing an unvoiced alveolar plosive, and you wanted to write the voiced alveolar plosive, you could write `[t +voiced]`. This starts a phoneme, passes all the attributes in `t`, and then sets `voiced` to `+`. 

### Filters

Anywhere where you are "matching" phonemes, such as in the input of a rule or in the environment, you can use a filter instead of a full phoneme. They let you pass a set of attributes, and it will match anything that fulfills all of those attributes, regardless of what the rest of it is. This is where negative parameters come in: if you put a negative parameter in a filter, it will match anything that is *not* the specified variant. Unfortunately, parameter attributes in filters will also override like in phonemes, so you can only match either a single negative or positive variant for each parameter. This will be changed in later versions.

Filters are defined with a space separated list of attributes inside of `(...)`. They can also be passed characters and selected phonemes, like phoneme atoms. For example, if you wanted to match all rounded vowels, you could write `(+type.vowel +rounded)`. If you want to match all consonants that are not velar, you could write `(+type.consonant -place.velar)`.

### Selectors

In the input of a rule, you can put a number in front of a filter to turn it into a selector. When a phoneme is matched by that filter, it will be assigned to that number in the output. You can either use it on its own, or you can pass it as an attribute in a phoneme to modify it with other attributes. For example, if you want to voice all fricatives, you could write `1(+type.consonant +manner.fricative)` in the input, and then in the output you could write `[1 +voiced]`. The numbers do not need to be contiguous or in order, they can be any natural number.

## Characters

To allow more natural, and concise specifications, Pawe lets you specify characters for certain phonemes, and then specify single character diacritics for individual attributes. While they're called characters, they don't necessarily need to be one Unicode codepoint, and can be any Unicode string that does not contain any character in `{}()[]/>*?!;` or a defined diacritic. After defining a character, you can use it anywhere where you can use a phoneme, or pass it into an atom for modification. You can define characters in a `characters` block like this:

    characters {
        t [+type.consonant +place.alveolar +manner.plosive -voiced];
        u [+type.vowel +height.close +backness.back +rounded];
    }

Defining characters also helps improve the output of Pawe, because it can automatically represent phonemes as a character with diacritics or find the best character to modify. In this case, optimal means the least amount of additional attributes. There is no guarantee of how ties will be broken, or that the order will be the same in every invocation of the program.

### Diacritics

In addition to characters, you can define diacritics that can be suffixed to characters to modify them. You can use as many diacritics on a given character as you want, and inputs are automatically decomposed so the diacritics in precomposed characters will still be processed. Diacritics can be any single Unicode character after NFD normalization. While this does include combining diacritics, like `◌́`, it also includes non-combining characters, like `ː` or `ʰ`.

Diacritics are defined in a `diacritics` block and must be preceeded by an arbitrary single Unicode character (after NFD normalization) to make combining diacritics easier to define. For example, if you wanted to use `◌́` to represent a stressed phoneme and `ː` to represent a long phoneme, you could write 

    diacritics {
        ó +stressed;
        oː +length.long;
    }

Note that we used `o` as the base character, but we could just have easily have used anything else. We could've also written

    diacritics {
        ź +stressed;
        zː +length.long;
    }

When preparing diacritics for display, PAWE will maintain the order in which diacritics are defined in the configuration, and then apply NFC normalization.

## Languages

In order to evolve to and from a language, you must first define its existence with a `languages` block. While Pawe could create language nodes as they're used, requiring the user to explicitly define the languages they're using keeps the file clear and tidy. It also prevents typos from silently disconnecting evolutionary chains. For example, if you wanted to make a configuration file that uses Proto-Indo-European, Proto-Germanic, and Proto-West GErmanic, you could write:

    languages {
        ProtoIndoEuropean;
        ProtoGermanic;
        ProtoWestGermanic;
    }
    
## Rules

Evolution rules are composed of 2-4 groups of atoms: an input, an output, an optional pre-environment, and an optional post-environment, written as `input > output / pre _ post;`, with an optional `#` marking word boundaries in the environment. Rules in an evolution ruleset are applied along the word once to prevent infinite loops. They are applied left-to-right and after applying a rule, Pawe will start again from the phoneme right after the replaced section, i.e. the first phoneme of the post-environment.

The following sections will describe these groups in more detail. For example, if you wanted to nasalize all vowels before /n/ at the end of a word (e.g. /kan/ → /kãn/), you could write:

    1(+type.vowel) > [1 +nasalized] / _ n #;

This will look for all vowels that are followed with /n/ and then the end of the word. If it finds one, it will assign it to `1`, and then replace it with `[1 +nasalized]` which takes whatever `1` is and sets `nasalized` to positive. Because `n` is in the post-environment instead of the input, it won't get touched. If, however, you want that /n/ to be removed in the process (e.g. /kan/ → /kã/), you can move it to the input so that it gets replaced:

    1(+type.vowel) n > [1 +nasalized] / _ #;

### Input

The input is the phonemes that Pawe will replace. It can contain an arbitrary mix of phonemes, filters, and selectors. At least one atom must be present in the input, but there is no upper length limit. Unfortunately, you cannot reference selected phonemes within the input, though this might be added in later versions.

### Output

The output is the phonemes that Pawe will replace the input with. It can only contain phonemes, but does not have to be the same length as the input, and can be empty if you just want to remove the input, though you still need to write the `>`. In the output, you can use reference selected phonemes by using their number. 

### Environment

The environments describe what must be present before and after the input in order for the rule to take effect. They can be phonemes or filters. Unfortunately, you cannot use selectors in the environment or reference selected phonemes. If you don't need an environment, you can just end a rule after the output with `;`. If you need either or both environments, you can write `/` to start the environment specification. The token `_` represents the position of the input in the word. If you only want a phoneme to apply at the beginning or end of a word, then you can use `#` on the corresponding end of the environment.
