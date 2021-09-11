<a id='x-28ANAFANAFO-3A-40INDEX-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

# Anafanafo, Common Lisp implementation!

## Table of Contents


###### \[in package ANAFANAFO with nicknames ANAFANAFO/CORE\]
This library is implementation of JavaScript version of [anafanafo](https://github.com/metabolize/anafanafo).

It is useful, when you want to know how long in pixels will be the string when the browser will
render it in a given font.

Library uses prebuilt data about character widths from the [original repository](https://github.com/metabolize/anafanafo).
It supports these fonts:

- Verdana Normal

- Verdana Bold

- Helvetica Bold

This implementation differ from the JavaScript version.
It returns text width for any font size, automatically applying
needed math transformation. All you need is to specify font family
and weight.

Default font is "Helvetica Bold 16px".

To use it, you need to load data using the [`LOAD-DATA`][994f] function:

<a id='x-28ANAFANAFO-3ALOAD-DATA-20FUNCTION-29'></a>

- [function] **LOAD-DATA** *&KEY (FAMILY \*DEFAULT-FONT-FAMILY\*) (WEIGHT \*DEFAULT-FONT-WEIGHT\*) (SIZE \*DEFAULT-FONT-SIZE\*)*

    Loads data for specified font name.
    
    Returns an object which can be used to retrieve text width:
    
    ```lisp
    
    CL-USER> (anafanafo:load-data :family "Verdana"
                                  :weight "bold")
    #<ANAFANAFO::DATA "Verdana" "bold" 16px :file "data/verdana-10px-bold.json">
    
    ```


Then you can calculate the width of the string:

<a id='x-28ANAFANAFO-3ASTRING-WIDTH-20FUNCTION-29'></a>

- [function] **STRING-WIDTH** *DATA TEXT*

    Returns width of the text in pixels.
    
    Result just a sum of all text characters:
    
    ```lisp
    CL-USER> (let ((data (anafanafo:load-data :family "Verdana"
                                              :weight "normal")))
               (anafanafo:string-width data
                                       "борщ"))
    43.70909
    CL-USER> (let ((data (anafanafo:load-data :family "Verdana"
                                              :weight "normal")))
               (+ (anafanafo:char-width data
                                        #б)
                  (anafanafo:char-width data
                                        #о)
                  (anafanafo:char-width data
                                        #р)
                  (anafanafo:char-width data
                                        #щ)))
    43.70909
    ```


Or width of a single character:

<a id='x-28ANAFANAFO-3ACHAR-WIDTH-20FUNCTION-29'></a>

- [function] **CHAR-WIDTH** *DATA CHAR &KEY (GUESS T)*

    Returns a float width of given char. Width is measured in pixels.
    
    ```lisp
    CL-USER> (let ((data (anafanafo:load-data :family "Verdana"
                                              :weight "normal")))
               (values (cons #щ
                             (anafanafo:char-width data #щ))
                       (cons #i
                             (anafanafo:char-width data #i))))
    (#CYRILLIC_SMALL_LETTER_SHCHA . 14.196364)
    (#i . 4.3927274)
    ```


  [994f]: #x-28ANAFANAFO-3ALOAD-DATA-20FUNCTION-29 "(ANAFANAFO:LOAD-DATA FUNCTION)"

* * *
###### \[generated by [40ANTS-DOC](https://40ants.com/doc)\]

