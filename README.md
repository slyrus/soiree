
soiree, A Common Lisp library for working with the vcard and icalendar
(and related) formats.

By Cyrus Harmon <ch-lisp@bobobeach.com>, February 2012. See COPYRIGHT
file for license details.

# Overview

Soiree is a library for working with information about individuals,
events, calendars and related pieces of information. It provides
parsers for the VCARD and iCalendar formats and represents the
information in XML documents corresponding to the xCard and xCal
specifications.

# Prerequisites

Soiree requires the parser-combinators, cxml-stp, cxml-rng, and
cl-base64 libraries. These can be loaded from quicklisp as follows:

    ;; (ql:quickload '(parser-combinators cxml-stp cl-base64))

    (ql:quickload '(parser-combinators cxml-stp cxml-rng cl-base64))

NOTE!!! Ugly hack alert!!! The version of cxml-rng currently in
Quicklisp, as of 2016-02-24, is from 2008 and does not work with the
current release of SBCL. An optimization to the gray-stream handling code in SBCL exposed a bug in cxml-rng, which prevents cxml-rng from loading. A version of cxml-rng which contains the necessary fix can be found here:

    http://www.lichteblau.com/git/cxml-rng.git

Use that for now. An easy way to do that is as follows:

    #!/bin/bash
    cd ~/quicklisp/local-projects
    git clone http://www.lichteblau.com/git/cxml-rng.git

instead of (ql-quickload 'cxml-rng)

# Getting started:

To load soiree:

    (asdf:load-system 'soiree)

# A quick example

Here's an example of using soiree to parse a simple vCard:

    (asdf:load-system 'soiree)

    (cl:defpackage #:soiree-example
      (:use #:cl #:soiree))

    (cl:in-package #:soiree-example)

    (defparameter *baba-oriley-vcard*
      (convert-string-to-dos-line-endings 
       "BEGIN:VCARD
    VERSION:3.0
    N:O'Riley;Baba;;;
    FN:Baba O'Riley
    ORG:Polydor Records
    TITLE:Field Worker
    PHOTO;VALUE=URL;TYPE=GIF:http://www.example.com/dir_photos/my_photo.gif
    ADR;type=WORK;type=pref:;;Trafalgar Square;London;England;;UK
    TEL;TYPE=WORK,VOICE:(415) 555-1212
    TEL;TYPE=HOME,VOICE:(415) 555-1213
    EMAIL;TYPE=PREF,INTERNET:thewho@example.com
    END:VCARD
    "))

    (defparameter *baba* (parse-vcard *baba-oriley-vcard*))

Doing this at the REPL, we see:

    SOIREE-EXAMPLE> (defparameter *baba* (parse-vcard *baba-oriley-vcard*))
    *BABA*
    SOIREE-EXAMPLE> *baba*
    #.(CXML-STP-IMPL::DOCUMENT
       :CHILDREN '(#.(CXML-STP:ELEMENT
                      #| :PARENT of type DOCUMENT |#
                      :EXTRA-NAMESPACES '(("" "urn:ietf:params:xml:ns:vcard-4.0"))
                      :CHILDREN '(#.(CXML-STP:ELEMENT
                                     #| :PARENT of type ELEMENT |#
                                     :CHILDREN '(#.(CXML-STP:ELEMENT
                                                    #| :PARENT of type ELEMENT |#
                                                    :CHILDREN '(#.(CXML-STP:ELEMENT
                                                                   #| :PARENT of type ELEMENT |#
                                                                   :CHILDREN '(#.(CXML-STP:TEXT
                                                                                  #| :PARENT of type ELEMENT |#
                                                                                  :DATA "O'Riley"))
                                                                   :LOCAL-NAME "surname"
                                                                   :NAMESPACE-URI "urn:ietf:params:xml:ns:vcard-4.0")
                                                                #.(CXML-STP:ELEMENT
                                                                   #| :PARENT of type ELEMENT |#
                                                                   :CHILDREN '(#.(CXML-STP:TEXT
                                                                                  #| :PARENT of type ELEMENT |#
                                                                                  :DATA "Baba"))
                                                                   :LOCAL-NAME "given"
                                                                   :NAMESPACE-URI "urn:ietf:params:xml:ns:vcard-4.0")

    ...

    SOIREE-EXAMPLE> 

# References:

VCARD specification Version 3:
[http://tools.ietf.org/html/rfc2426](http://tools.ietf.org/html/rfc2426)

VCARD specification Version 4:
[http://tools.ietf.org/html/rfc6350](http://tools.ietf.org/html/rfc6350)

xCard specification:
[http://tools.ietf.org/html/rfc6351](http://tools.ietf.org/html/rfc6351)

