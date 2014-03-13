;;; rel8r-sqlite.scm -- SQLite3 interface for the Rel8r tool.
;;;   Copyright © 2014 by Matthew C. Gushee <matt@gushee.net>
;;;   This program is open-source software, released under the GNU General
;;;   Public License v3. See the accompanying LICENSE file for details.

(module rel8r-store
        *
        (import scheme chicken)
        (use sql-de-lite)
        (use s11n)

;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  UTILITY FUNCTIONS  -----------------------------------------------

(define (eprintf msg . args)
  (error (apply sprintf `(,msg ,@args))))

(define db->integer string->number)

(define db->float string->number)

(define db->string identity)

(define (db->boolean dbval)
  (cond
    ((string=? dbval "0") #f)
    ((string=? dbval "1") #t)
    (else (eprintf ("'~A' is not a boolean value")))))

(define (db->datetime dbval)
  (with-input-from-string dbval
    (lambda ()
      (deserialize))))

(define integer->db number->string)

(define float->db number->string)

(define string->db identity)

(define (boolean->db b)
  (if b "1" "0"))

(define (datetime->db dt)
  (with-output-to-string
    (lambda ()
      (serialize dt))))

(define validate-integer integer?)

(define validate-float flonum?)

(define validate-boolean boolean?)

(define validate-string string?)

(define (validate-datetime dt)
  (or (date? dt)
      (time? dt)
      (and (list? dt)
           (= (length dt) 2)
           (date? (car dt))
           (time? (cadr dt)))))
           

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO


(define create-type-table-query
  "CREATE TABLE types (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT UNIQUE NOT NULL
  );")

(define populate-type-table-queries
  '("INSERT INTO types (name) VALUES ('integer');"
    "INSERT INTO types (name) VALUES ('float');"
    "INSERT INTO types (name) VALUES ('boolean');"
    "INSERT INTO types (name) VALUES ('string');"
    "INSERT INTO types (name) VALUES ('pstring');"
    "INSERT INTO types (name) VALUES ('term');"
    "INSERT INTO types (name) VALUES ('date');"
    "INSERT INTO types (name) VALUES ('time');"
    "INSERT INTO types (name) VALUES ('datetime');"
    "INSERT INTO types (name) VALUES ('nref');"
    "INSERT INTO types (name) VALUES ('rref');"
    "INSERT INTO types (name) VALUES ('xref');"))

(define create-pstring-table-query
  "CREATE TABLE pattern_strings (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL,
    pattern TEXT NOT NULL,
    description TEXT
  );")

(define create-vocab-table-query
  "CREATE TABLE vocabs (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL,
    term TEXT NOT NULL,
    description TEXT
  );")

(define create-triple-table-query
  "CREATE TABLE triples (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    s  TEXT NOT NULL,
    p  TEXT NOT NULL,
    t  INTEGER REFERENCES types(id) NOT NULL,
    o  TEXT NOT NULL,
    pname INTEGER REFERENCES pattern_strings(id),
    vocab TEXT REFERENCES vocabs(name) 
  );")

(define create-cardinality-table-query
  "CREATE TABLE cardinalities (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL
  );")

(define populate-cardinality-table-queries
  '("INSERT INTO cardinalities (name) VALUES ('one');"
    "INSERT INTO cardinalities (name) VALUES ('zoo');"
    "INSERT INTO cardinalities (name) VALUES ('zoma');"
    "INSERT INTO cardinalities (name) VALUES ('ooma');"))

(define create-struct-type-table-query
  "CREATE TABLE struct_types (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL,
    relname TEXT NOT NULL,
    any_type INTEGER DEFAULT 0,
    cardinality INTEGER REFERENCES cardinalities(id) NOT NULL
  );")

(define create-struct-obj-type-table-query
  "CREATE TABLE struct_object_types (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    struct_rel INTEGER REFERENCES struct_types(id),
    obj_type INTEGER REFERENCES types(id),
    pstring INTEGER REFERENCES pattern_strings(id),
    vocab TEXT REFERENCES vocabs(name)
  );")

(define (create-db filename)
  (let* ((db
          (open-database filename))
         (qx
          (lambda (q)
            (let ((s (sql/transient db q)))
              (exec s)))))
    (for-each
      qx
      `(,create-type-table-query
        ,create-pstring-table-query
        ,create-vocab-table-query
        ,create-triple-table-query
        ,create-cardinality-table-query
        ,create-struct-type-table-query
        ,create-struct-obj-type-table-query))
    (for-each qx populate-type-table-queries)
    (for-each qx populate-cardinality-table-queries)
    (close-database db)))

) ; END MODULE

;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ------------------------------------------------------------------------

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

;;; ========================================================================
;;; ------------------------------------------------------------------------

