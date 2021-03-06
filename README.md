<a id='x-28ANAFANAFO-3A-40INDEX-20MGL-PAX-MINIMAL-3ASECTION-29'></a>

# Anafanafo, Common Lisp implementation!

## Table of Contents


###### \[in package ANAFANAFO with nicknames ANAFANAFO/CORE\]
This library is implementation of JavaScript version of [anafanafo](https://github.com/metabolize/anafanafo).

It is useful, when you want to know how long in pixels will be the string when the browser will
render it in a given font.

Library uses prebuilt data about character widths from the [original repository](https://github.com/metabolize/anafanafo).
It supports these fonts:

- Verdana Normal 10px

- Verdana Bold 10px

- Verdana Normal 11px

- Helvetica Bold 11px

To use it, you need to load data using the [`LOAD-DATA`][994f] function:

<a id='x-28ANAFANAFO-3ALOAD-DATA-20FUNCTION-29'></a>

- [function] **LOAD-DATA** *&KEY (FAMILY \*DEFAULT-FONT-FAMILY\*) (WEIGHT \*DEFAULT-FONT-WEIGHT\*) (SIZE \*DEFAULT-FONT-SIZE\*)*

    Loads data for specified font name.
    
    Returns an object which can be used to retrieve text width:
    
    ```lisp
    CL-USER> (load-data "Verdana Normal 10px")
    
    #<DATA "Verdana Normal 10px" :file "data/verdana-10px-normal.json">
    ```


Then you can calculate the width of the string:

<a id='x-28ANAFANAFO-3ASTRING-WIDTH-20FUNCTION-29'></a>

- [function] **STRING-WIDTH** *DATA TEXT*

    Returns width of the text in pixels.
    
    Result just a sum of all text characters:
    
    `CL-USER`> (let ((data (anafanafo:load-data "Verdana Normal 10px")))
               (anafanafo:string-width data
                                       "борщ"))
    27.32
    `CL-USER`> (let ((data (anafanafo:load-data "Verdana Normal 10px")))
               (+ (anafanafo:char-width data
                                        #б)
                  (anafanafo:char-width data
                                        #о)
                  (anafanafo:char-width data
                                        #р)
                  (anafanafo:char-width data
                                        #щ)))
    27.32

Or width of a single character:

<a id='x-28ANAFANAFO-3ACHAR-WIDTH-20FUNCTION-29'></a>

- [function] **CHAR-WIDTH** *DATA CHAR &KEY (GUESS T)*

    Returns a float width of given char. Width is measured in pixels.
    
    `CL-USER`> (let ((data (anafanafo:load-data "Verdana Normal 10px")))
               (values (cons #щ
                             (anafanafo:char-width data #щ))
                       (cons #i
                             (anafanafo:char-width data #i))))
    (#CYRILLIC\_SMALL\_LETTER\_SHCHA . 8.88)
    (#i . 2.74)

  [994f]: #x-28ANAFANAFO-3ALOAD-DATA-20FUNCTION-29 "(ANAFANAFO:LOAD-DATA FUNCTION)"

* * *
###### \[generated by [MGL-PAX](https://github.com/melisgl/mgl-pax)\]
