(ns de.skamphausen.stt14
  (:require [instaparse.core  :as i]
            [instaparse.combinators :as c]
            [clojure.java.io  :as io]
            [clojure.xml      :as xml]
            [clojure.string   :as s]))

;;; Parsing

;;; The Canonical Resources
;; http://de.wikipedia.org/wiki/Parser
;; http://en.wikipedia.org/wiki/Parsing


;;; Description
;; Lexical analysis (tokenization)
;; Syntactic analysis (parse-tree)

;; Startsymbol + production rules
;; Terminal and non-terminal symbols
;; Context-free grammar (on left side only non-terminals)



;;; Different Types
;; Top-down vs bottom-up
;; LR(k), LL(k) (left-/right-reduction), and many more


;;; Well Known Implementations
;; Yacc, ANTLR, Bison, uvm


;;; GLL Parsing
;; Instaparse
;; http://dotat.at/tmp/gll.pdf
;; http://www.codecommit.com/blog/scala/unveiling-the-mysteries-of-gll-part-1
;; http://www.codecommit.com/blog/scala/unveiling-the-mysteries-of-gll-part-2



;;; ... but you probably know the theory better than me anyway
;; so let's focus on using Instaparse


;;; 2014
;; Given the widespread usage of JSON, XML and CSV it is not so easy
;; to find relevant plain text formats today.  Do not try to use
;; Instaparse for formats for which specialized libraries exist.


;;; Introduction Example
;; That's German for 'Instant Fun With Instaparse' but it sounds 
;; better: Spaß rhymes with parse pretty well. 
(def talk-title "Instant Spaß mit Instaparse.")

(def title-grammar-1
  "sentence = words DOT
   DOT      = '.'
   words    = word (SPACE word)*
   SPACE    = ' '
   word     = #'(?U)\\w+'")

;; i/parser creates a parser from the grammar
;; Resulting parser can be called like a function
;; Implementation detail: it is not a function but a Record which
;;   implements the suitable interface
(defn test-title-parser [grammar]
  ((i/parser grammar) talk-title))

;; Output has too much information 
;; <> come to the rescue and suppress the dot
;; Go ahead, test it at the REPL
(def title-grammar-2
  "sentence = words DOT
   DOT      = <'.'>
   words    = word (SPACE word)*
   SPACE    = ' '
   word     = #'(?U)\\w+'")

;; What happens with < and > on the left hand side?
(def title-grammar-3
  "sentence = words DOT
   DOT      = <'.'>
   words    = word (SPACE word)*
   SPACE    = ' '
   <word>   = #'(?U)\\w+'")

;; and what about both sides?
(def title-grammar-4
  "sentence = words DOT
   DOT      = <'.'>
   words    = word (SPACE word)*
   <SPACE>  = <' '>
   <word>   = #'(?U)\\w+'")


;;; And Now For Some Real-World Data

;;; CDDB

;; An example
(def cddb-example
  (slurp "resources/data/cddb.710b2b08"))

;; Simplified grammar based upon
;; http://www.robots.ox.ac.uk/~spline/cddb-howto.txt
(def cddb-grammar
  "start     = line+
   line      = (comment | discdata) EOL
   EOL       = '\n' | '\r\n'
   comment   = #'^#.*'
   discdata  = name EQ data
   EQ        = '='
   name      = #'[A-Z0-9]+'
   data      = #'[\\x20-\\x7eh\\xA0h-\\xFFh]*'")

(defn test-cddb-parser [grammar]
  ((i/parser grammar) cddb-example))

;; Again, clean up a little
(def cddb-grammar-clean
  "<start>   = line+
   <line>    = (comment | discdata) EOL
   <EOL>     = <'\n'> | <'\r\n'>
   <comment> = <#'^#.*'>
   discdata  = name EQ data
   <EQ>      = <'='>
   <name>    = #'[A-Z0-9]+'
   <data>    = #'[\\x20-\\x7eh\\xA0h-\\xFFh]*'")

;;; Transform

;; Instaparse allows transformation of the results on-the-fly

;; Take start back in
(def cddb-grammar-transform
  "start     = line+
   <line>    = (comment | discdata) EOL
   <EOL>     = <'\n'> | <'\r\n'>
   <comment> = <#'^#.*'>
   discdata  = name EQ data
   <EQ>      = <'='>
   <name>    = #'[A-Z0-9]+'
   <data>    = #'[\\x20-\\x7eh\\xA0h-\\xFFh]*'")

;; Turn key value pair into hash map
(defn discdata->map [name data]
  {(keyword name) data})

;; & collects all args in a list
(defn merge-lines [& all-lines]
  (into {} all-lines))

;; This is the same parser as in the previous example
;;   but we use ->> for convenience
;; Why do we need the extra pair of parens?
;; Why ->> and not ->?
;; Transformation step is last
(defn test-clean-cddb-reader [grammar]
  (->> "resources/data/cddb.710b2b08"
       slurp
       ((i/parser grammar))
       (i/transform {:discdata discdata->map
                     :start    merge-lines})))



;;; PGN Chess
;; http://www.thechessdrum.net/PGN_Reference.txt

;; Example data
(def magnus-carlsen-pgn 
  (slurp "resources/data/carlsen.pgn"))

;; This simplifies the grammar of a move.  See e.g.
;; http://pyparsing.wikispaces.com/file/view/pgn.py/30112820/pgn.py
;; for a parser in Python which deals with the various forms of a
;; move.
(def pgn-grammar 
  "
PGNDatabase = PGNGame+

PGNGame      = Tag+ ws+ Moves ws*

Tag          = ws* <'['> TagName ws QU TagValue QU <']'> NL

TagName      = #'[^\\s]+'

TagValue     = #'[^\"]*'

Moves        = Move+ GameTermination

Move         = Number Element ws Element ws

<Number>     = #'[0-9]+\\.'
<Element>    = #'[PNBRQK]?[a-hx0-9+]+' 
                 | KingCastling 
                 | QuenCastling
KingCastling = <'O-O'>
QuenCastling = <'O-O-O'>

GameTermination = WhiteWins | BlackWins | Draw | Unknown

WhiteWins = <'1-0'>
BlackWins = <'0-1'>
Draw      = <'1/2-1/2'>
Unknown   = <'*'>

<ws> = <#'\\s+'>
<NL> = <#'\\r?\\n'>
<QU> = <'\"'>")


;; Instaparse supports a second output method: enlive.
;; So far we've used hiccup.
;; Both are common tree structures in the Clojure world.
(defn read-pgn-database []
  ((i/parser pgn-grammar
            :output-format :enlive)
   magnus-carlsen-pgn))

(defn pgn->xml []
  (binding [*out* (java.io.FileWriter. "/tmp/pgn.xml")]
    (xml/emit (read-pgn-database))))
;; Let's open that file ...
;; Or use the shell: xmlindent /tmp/pgn.xml | less




;;; Apache

;; Very simplified Apache log
;; Would read a sequence of lines from a file with line-seq
;; This kind of file is too large for Instaparse
;; Just parse each line with Instaparse
(def apache-log
  ["10.10.30.56 [21/Jul/2014:01:10:25 +0200]"
   "10.10.30.56 [21/Jul/2014:01:10:25 +0200]"
   "10.10.30.78 [21/Jul/2014:14:39:41 +0200]"
   "10.10.30.78 [21/Jul/2014:14:39:43 +0200]"
   "10.10.30.78 [22/Jul/2014:14:01:08 +0200]"])

;; Start with a simplified grammar for an IP address
(def grammar-ip 
  "ip   = n d n d n d n
    <n> = #'[0-9]+'
    <d> = <'.'>")
(comment
  ((i/parser grammar-ip) "10.10.30.78"))

;; And here is a grammar for the timestamp
;; Sure, that's simplified as well
(def grammar-date
  "datetime = <'['> date <':'> time <' '> tz <']'>
   date  = #'\\d+/\\w+/\\d+'
   time  = #'\\d+:\\d+:\\d+'
   tz    = #'[+-]\\d+'")
(comment
  ((i/parser grammar-date) "[22/Jul/2014:14:01:08 +0200]"))
;; There is a difference regarding greedyness for 
;; - instaparse grammar
;; - and regexps.
;; So, no worries about the colon.

;; Now leverage the combinator library of Instaparse
;;   instaparse.combinators
;; Use ebnf to translate a EBNF grammar into a tree structure
(comment 
  (c/ebnf grammar-ip))

;; Result of ebnf is just a hash map
;; Can be combined as usual
(def combined
  (merge
   {:log (c/cat (c/nt :ip) (c/string " ") (c/nt :datetime))}
   (c/ebnf grammar-ip)
   (c/ebnf grammar-date)))

;; Hash map has to name the start rule which so far always
;; was simply the first rule in the grammar
(comment
  ((i/parser combined :start :log) (first apache-log)))



;;; Can't We Build This Dynamically? I hear you say?
;; Sure we can


;;; Build a Logfile Parser From The Log Definition

;; Apache config contains LogFormat
;; This one matches the simplified logs above
(def apache-log-def "LogFormat \"%a %t\"")

;; Need a parser for the LogFormat
(def grammar-log-def
  "fmt = <'LogFormat '> quote declaration quote
   <quote>     = <'\"'>
   declaration = (char+ | fmtstr)+
   char        = #'[\\w ]+'
   <fmtstr>    = time | ip
   ip          = <'%a'>
   time        = <'%t'>")

(defn parse-format-def [fmt-def]
  ((i/parser grammar-log-def) fmt-def))

(comment 
  (parse-format-def apache-log-def))


;; Multi Method with first as dispatch function
(defmulti log-decl->grammar first)

(defmethod log-decl->grammar :ip 
  [x]
  (c/ebnf grammar-ip))

(defmethod log-decl->grammar :time
  [x]
  (c/ebnf grammar-date))

(defmethod log-decl->grammar :char
  [x]
  (c/string (second x)))

(comment
 (log-decl->grammar [:ip])
 (log-decl->grammar [:char " "]))

;; Need some helper methods
(defn key-or-string 
  "Re-use strings as-is, and make others a non-terminal."
  [decls]
  (map (fn [x] 
         (if (= :tag (key (first x))) 
           x 
           (c/nt (key (first x))))) 
       decls))


(defn nt-decls 
  "Get only the non terminal declarations."
  [decls]
  (filter
   (complement
     #(= :tag (key (first %)))) decls ))

(defn make-log-parser [& args]
  (let [decls (map log-decl->grammar args)
        line  (apply c/cat (key-or-string decls))
        gram  (nt-decls decls)]
    (i/parser (merge 
               {:line line}
               (into {} gram))
              :start :line)))

(defn logdef->parser [logdef]
  (second
   (i/transform {:declaration make-log-parser}
                (parse-format-def logdef))))

;;; Closing Notes
;; Quantifiers in regexps are greedy; in grammars they consume all
;;   possible numbers of repetitions. This can make a grammar
;;   ambiguous.
;; Ambiguous grammars can be analyzed with i/parses.
;;
;; Try to make your grammar non-ambigous.  Performance penalties ahead.
;;
;; Too many non-terminals in the grammar are bad for
;;   performance, too 
;; 
;; Some PEG extensions are supported
;;
;; Partial parses that do not consume the complete input string are
;; supported.
;;
;; Total parsing mode allows parsing to finish and make the failure
;; and the remaining input part of the result.  Useful for debugging.
;;
;; Experimental support for an automatic whitespace handling
;; available.  Useful when translating from e.g. flex.
;; 
;; Instaparse is Self-Hosted: the parser for the A/EBNF strings is
;; written in the tree structure returned by c/ebnf
;;
;; Adding rhizome to your deps and installing graphviz enables
;; visualizations:
(comment
  (i/visualize ((i/parser title-grammar-1)
                "Viel Spaß mit Instaparse."))
  ;; or
  (i/visualize ((i/parser title-grammar-4)
                "Viel Spaß mit Instaparse.")))
