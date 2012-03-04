
Ever since taking [Stuart
Russell](http://www.cs.berkeley.edu/~russell/)'s [Knowledge
Representation and Reasoning
class](http://www.cs.berkeley.edu/~russell/classes/cs289/f01/) (Holy
smokes, that was my first real introduction to Common Lisp and that
was eleven years ago! How time flies...), I've always felt like I
don't really _have_ my data unless I have it in some machine readable
form -- and not just in an unstructured text document or in some
proprietary GUI application, but rather in a place where I can
reasonably query, update, search, etc... the data.

So, one kind of data that I haven't really _had_ is my address
book. For a while I kept things locally in Apple's Address Book
application, but that really only worked for a single machine, at
least at first. At some point I discovered the awesomeness of
[DAViCal](http://www.davical.org/), which is a CALDAV/CARDDAV server
for serving up calendar and contact/address book information. Great!
OK, so, I jumped through the requisite hoops to get things working
between DAViCal and Address Book (and iCal) and all was good -- except
for the fact that I still didn't have a nice convenient API for
searching/querying/retrieving/updating the data in DAViCal.

So, that motivated me to write some code for working with my data from
Address Book and from DAViCal, which leads me to this blog post. It
turns out that there is an IETF standard for exchanging contact
information, the [VCARD
format](http://tools.ietf.org/html/rfc6350). Apparently I can export
and import data from Address Book in this form, and this is how the
CardDAV data is stored in DAViCal, I think. Great. Now all I need to
do is to parse the VCARD data and I'm good to go. Fortunately, the
spec lays out the file format nicely and this shouldn't be too
terribly hard to parse.

* Data Model?

But, that leads to the next question of, having parsed the data, what
I'm I going to _do_ with the data? Or, put another way, how am I going
to represent/model the data contained in the VCARD file? Just slurping
the VCARD bits into a buffer of characters doesn't help my find, for
instance, the email address of a particular person. One approach would
be to define a data model with CLOS classes and generic functions that
operate on those classes to allow for reading/writing/querying the
data. Another approach would be to use a more generic data structure
like lists or, probably better yet, nested hash-tables that would
allow traversal of a graph of data objects via key/value
relationships. The VCARD format itself could be that of as a (somewhat
unwieldy) data model.

* The VCARD Specification

The [VCARD 3.0 specification](http://tools.ietf.org/html/rfc6350)
formally describes what VCARD data should look like. The problem is
that transferring back and forth either between CLOS objects or
hash-tables and VCARD data sounds like a cumbersome and error-prone
process, especially as either the objects or hash-tables, on the one
hand, or the VCARD format itself evolves over time. Surely there's a
better approach.

The other problem with the VCARD specification is that the
specification isn't machine readable. Yes, there are bits of
[ABNF](http://tools.ietf.org/html/rfc5234) grammars in there, but the
spec, as a whole, isn't easily parsed and queried. If one was going to
rely on a specification to define the data model, it sure would be
nice if the specification itself could be read, interrogated, and used
by our programs.

* xCard: VCARD XML Representation

Enter the [xCard
specification](http://tools.ietf.org/html/rfc6351). The
xCard specification does two things, it provides a model for reading
and writing vCard data (that is the data that can be represented in
VCARDs, not the actual VCARD syntax) and, more importantly, it
provides a machine-readable representation of the specification in the
form of a [RELAX NG
schema](http://tools.ietf.org/html/rfc6351#appendix-A).

* RELAX NG

[RELAX
NG](http://relaxng.org/) itself is an XML schema for describing XML
schemas. It provides for a human-friendly but machine-readable format
for specifying RELAX NG schemas, the [RELAX NG Compact
Syntax](http://relaxng.org/compact-20021121.html).

* Back to xCard

So, we find the compact syntax for the [xCard
schema](http://tools.ietf.org/html/rfc6351#appendix-A) at the end of
the standard. For whatever reason, I can't locate a canonical version
of the schema outside of the spec, but it's simple enough to cut and
paste the bits out of the spec and place it in an RNG file. Great. Now
we've got data in VCARD format, a nice machine-readable (and
human-friendly) description of the data model in the RELAX NG compact
syntax. How does this solve the problem of
representing/querying/modifying the data that I mentioned above? Well,
one approach would be automagically generate CLOS classes and
interfaces that match the RELAX NG schema. This seemed like an
interesting approach, but an awful lot of work. Since we've got a
schema that defines the vCard semantics, as represented by an XML
document, perhaps we can just use an in-memory representation of the
XML data itself as our "data model" for
reading/writing/querying/etc... the address book data. This is the
approach I've taken with cl-vcard, and we'll come back to it
momentarily.

* Parsing VCARDs parser-combinators

For the moment, before we get into what are we transforming the data
_to_, we need to consider what we're transforming the data _from_ and
how to do so. A simple, hand-coded recursive descent parser would
probably be the most straightforward way to go, but I'm exceedingly
lazy and wanted someone else to do the bulk of the heavy lifting of
parsing for me. Enter Jakub Higersberger's awesome
[parser-combinator]() library, inspired by Haskell's
[parsec](http://www.haskell.org/haskellwiki/Parsec) monadic
parser-combinator library.

While it may be overkill, parser-combinator provides for a nice, clean
API for writing parsers. The core of the parsing routine is shown below:

    (defun content-line? (&optional name)
      ;; [group "."] name *(";" param) ":" value CRLF
      (named-seq?
       (<- group (opt? (hook?
                        #'first
                        (seq-list? (group?) "."))))
       (<- name (if name name (name?)))
       (<- params (many? (named-seq?
                          ";"
                          (<- param (param?))
                          param)))
       ":"
       (<- value (value?))
       (<- long-lines (many? (long-line-extension?)))
       (seq-list? #\Return #\Newline)
       (when name
         (list group name params (apply #'concatenate 'string value long-lines)))))

* fset

Parser combinators are designed to work with functional data
structures, as it they may backtrack, and modifying data structures as
one goes with parser combinators can lead to problems. Therefore, I
use [fset](http://common-lisp.net/project/fset/), a functional
collections library, for building the parsed representation of the
data as I go. I could probably get away without doing this, but, it
seems like a good idea to use a functional parser as such, rather than
abusing the idea that we're not taking advantage of backtracking such
that we can modify our data structures along the way as we do the
parse.

* Putting it all together

Here's an example of using cl-vcard to parse a simple vCard:

    (asdf:load-system 'cl-vcard)

    (cl:defpackage #:cl-vcard-example
      (:use #:cl #:cl-vcard))

    (cl:in-package #:cl-vcard-example)

    (defparameter *baba-oriley-vcard*
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
    ")

    (defparameter *baba* (parse-vcard *baba-oriley-vcard*))

(note that the vcard string needs CRLF line endings!)

Doing this at the REPL, we see:

    CL-VCARD-EXAMPLE> (defparameter *baba* (parse-vcard *baba-oriley-vcard*))
    *BABA*
    CL-VCARD-EXAMPLE> *baba*
    #.(CXML-STP-IMPL::DOCUMENT
       :CHILDREN '(#.(CXML-STP:ELEMENT
                      #| :PARENT of type DOCUMENT |#
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

    ...

So we have a big, hairy document tree that (hopefully) has the vcard
data we want in it in a form that will, eventually, prove to be easy
for us to work with. But, before we get into actually doing anything
with the data, let's take a digression into XML data representation
and validation.

* STP, CXML, RNG, CXML-RNG

Notice that we're using
stp:serialize. [CXML-STP](http://www.lichteblau.com/cxml-stp/) is a
document-based interface to XML data, written by David Lichteblau, and
somewhat like the DOM, only with a much nicer interface (in my subject
opinion, of course), inspired by the [XOM](http://www.xom.nu/). A
comparison between the DOM and STP interfaces can be found
[here](http://www.lichteblau.com/cxml-stp/DOM-COMPARISON).

In the guts of the parser we use functions like stp:make-element and
stp:append-child to construct the document tree. These, and the rest
of the STP API, sit on top of Gilbert Baumann's [Closure XML (or CXML)
library](http://common-lisp.net/project/cxml/).

* What's the damn phone number?

OK, so far this has all been a lot of work and we haven't even gotten
to access any of our data. We're almost there. We just need one more
XML library (of course...). We _could_ work with the STP document directly:

    CL-VCARD-EXAMPLE> (stp:string-value
     (stp:find-child
      "text"
      (stp:find-child
       "tel"
       (stp:find-child
        "vcard"
        (stp:find-child 
         "vcards" *baba*
         :key #'stp:local-name :test 'equal)
        :key #'stp:local-name :test 'equal)
       :key #'stp:local-name :test 'equal)
      :key #'stp:local-name :test 'equal))
    "(415) 555-1212"

But this grows tedious. Fortunately, there's a better way.

* Plexippus-XPath

[Plexippus XPath](http://common-lisp.net/project/plexippus-xpath/) is
another excellent library in the CXML family, written by Ivan
Shvedunov. Plexippus provides an implementation of the XPath spec that
works with CXML documents. Using plexippus instead of walking the tree
by hand as above, we can do:

    (xpath:with-namespaces ((nil cl-vcard:*vcard-namespace*))
      (xpath:evaluate "string(/vcards/vcard/tel/*/text())" *baba*))

Ignoring the with-namespaces macro invocation for a moment, we see a single line of code that gets us the information we want. Win! This is a very simple example, but the XPath language allows us to write much more interesting queries. There are two housekeeping matters we need to take care of first.

First, let's make a macro to handle the namespace stuff. Plexippus is (rightfully) rather picky about making sure that XML element and attribute names are properly qualified. We can set the default namespace as above, but we'll do this in a macro in case we want to change this later:

    (defmacro with-vcard-namespace (&body body)
      `(xpath:with-namespaces ((nil cl-vcard::*vcard-namespace*))
         ,@body))

Second, we'll make a little function for converting XPath query
results to text. We'll probably use something else in an real
application using this stuff, but it's nice for playing around with
the API and for interactive development.

    (defun join-xpath-result (result)
      (if (xpath:node-set-p result)
          (format nil "~{~a~^~&~}"
                  (xpath:map-node-set->list #'xpath:string-value result))
          (xpath:string-value result)))

Now, back to XPath. If, for instance, I wanted to see the formatted names of every one in my family vcard database who has a photo in the database, I could do:

    (join-xpath-result
     (with-vcard-namespace
       (xpath:evaluate "/vcards/vcard[count(photo)>0]/fn/text" *family*)))

There's lots more one can do, but that should give you a flavor of how XPath can be used to effectively walk the document tree.

* Validation

As mentioned earlier, the xCard specification gives us a nice Relax NG
(RNG) schema for data about individuals and other entities (as the
spec somewhat vacuously says). The nice thing about this is that we
can use the schema to validate our in memory representation of the
xCard data -- even if there's never an xCard file per se:

    (stp:serialize *baba* (cxml-rng:make-validator *vcard-rng-schema*))

Once we've got that the document in place we can validate it against
the Relax NG schema. The VCARD -> xCard may not be complete (which it
isn't yet), but at least we know that the (so far tested) output is
valid XML, that complies with the Relax NG schema.

* Really putting it all together

Here's another simple example of getting some data out of the xCard document:

    CL-VCARD-EXAMPLE> (xpath:with-namespaces ((nil cl-vcard::*vcard-namespace*))
      (format nil "~A is a ~A who works at ~A and can be reached via e-mail at ~A"
              (xpath:evaluate "string(/vcards/vcard/fn/*/text())" *baba*)
              (xpath:evaluate "string(/vcards/vcard/title/*/text())" *baba*)
              (xpath:evaluate "string(/vcards/vcard/org/*/text())" *baba*)
              (xpath:evaluate "string(/vcards/vcard/email/*/text())" *baba*)))

    "Baba O'Riley is a Field Worker who works at Polydor Records and can be reached via e-mail at thewho@example.com"

Of course we can write more interesting queries, make a proper front end to the data, write it back out, talk to an address book server, etc... but those exercises are left for the reader.

Wait a minute, did I say talk to a server?
[DRAKMA](http://weitz.de/drakma/) would be perfect for that, but
there's one problem. In the next blog post, I'll go into how one can
talk to a CardDAV server and what one needs to change in DRAKMA to
make this work. Next time...

In the mean time, cl-vcard can be found on [github](https://github.com/slyrus/cl-vcard).

* References

** Libraries that made this all possible

1. [CXML](http://common-lisp.net/project/cxml/)

2. [CXML-STP](http://www.lichteblau.com/cxml-stp/)

3. [CXML-RNG](http://www.lichteblau.com/cxml-rng/)

4. [cl-parser-combinators](https://github.com/Ramarren/cl-parser-combinators)

5. [fset](http://common-lisp.net/project/fset/)

** Standards

1. [VCARD](http://tools.ietf.org/html/rfc6350)

2. [ABNF](http://tools.ietf.org/html/rfc5234)

3. [xCard](http://tools.ietf.org/html/rfc6351)

4. [Relax NG](http://relaxng.org/spec-20011203.html)

5. [RELAX NG Compact Syntax](http://relaxng.org/compact-20021121.html)

6. [XPATH](http://www.w3.org/TR/xpath/)

7. [DRAKMA](http://weitz.de/drakma/)