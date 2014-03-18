;;; rel8r-sqlite.scm -- SQLite3 interface for the Rel8r tool.
;;;   Copyright © 2014 by Matthew C. Gushee <matt@gushee.net>
;;;   This program is open-source software, released under the GNU General
;;;   Public License v3. See the accompanying LICENSE file for details.

(module rel8r-store
        *
        (import scheme chicken)
        (import extras)
        (import data-structures)
        (import ports)
        (use sql-de-lite)
        (use s11n)
        (use srfi-19)
        (use srfi-19-period)

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
      (time-period? dt)
      (and (list? dt)
           (= (length dt) 2)
           (date? (car dt))
           (time? (cadr dt)))))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  DATABASE SETUP  --------------------------------------------------

;;; ------  Queries  -------------------------------------------------------

(define create-primitive-table-query
  "CREATE TABLE primitives (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT UNIQUE NOT NULL
  );")

(define populate-primitive-table-queries
  '("INSERT INTO primitives (name) VALUES ('integer');"
    "INSERT INTO primitives (name) VALUES ('float');"
    "INSERT INTO primitives (name) VALUES ('boolean');"
    "INSERT INTO primitives (name) VALUES ('string');"
    "INSERT INTO primitives (name) VALUES ('date');"
    "INSERT INTO primitives (name) VALUES ('time');"
    "INSERT INTO primitives (name) VALUES ('datetime');"
    "INSERT INTO primitives (name) VALUES ('nref');"
    "INSERT INTO primitives (name) VALUES ('rref');"
    "INSERT INTO primitives (name) VALUES ('xref');"))

(define create-string-type-table-query
  "CREATE TABLE string_types (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL,
    pattern TEXT NOT NULL,
    description TEXT
  );")

(define create-number-type-table-query
  "CREATE TABLE number_types (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL,
    min FLOAT,
    max FLOAT,
    step FLOAT,
    digits INTEGER,
    description TEXT
  );")

(define create-vocab-table-query
  "CREATE TABLE vocabs (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL,
    term TEXT NOT NULL,
    description TEXT
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
    extensible INTEGER default 0
  );")

(define create-type-class-table-query
  "CREATE TABLE type_classes (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL
  );")

(define populate-type-class-table-queries
  '("INSERT INTO type_classes (name) VALUES ('primitive');"
    "INSERT INTO type_classes (name) VALUES ('string');"
    "INSERT INTO type_classes (name) VALUES ('number');"
    "INSERT INTO type_classes (name) VALUES ('vocab');"
    "INSERT INTO type_classes (name) VALUES ('struct');"
    "INSERT INTO type_classes (name) VALUES ('union');"))

(define create-types-table-query
  "CREATE TABLE types (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL UNIQUE,
    class INTEGER REFERENCES type_classes(id)
  );")

(define populate-types-table-queries
  '("INSERT INTO types (name, class) SELECT 'integer', id FROM type_classes WHERE name = 'primitive';"
    "INSERT INTO types (name, class) SELECT 'float', id FROM type_classes WHERE name = 'primitive';"
    "INSERT INTO types (name, class) SELECT 'boolean', id FROM type_classes WHERE name = 'primitive';"
    "INSERT INTO types (name, class) SELECT 'string', id FROM type_classes WHERE name = 'primitive';"
    "INSERT INTO types (name, class) SELECT 'date', id FROM type_classes WHERE name = 'primitive';"
    "INSERT INTO types (name, class) SELECT 'time', id FROM type_classes WHERE name = 'primitive';"
    "INSERT INTO types (name, class) SELECT 'datetime', id FROM type_classes WHERE name = 'primitive';"
    "INSERT INTO types (name, class) SELECT 'nref', id FROM type_classes WHERE name = 'primitive';"
    "INSERT INTO types (name, class) SELECT 'rref', id FROM type_classes WHERE name = 'primitive';"
    "INSERT INTO types (name, class) SELECT 'xref', id FROM type_classes WHERE name = 'primitive';"
    "INSERT INTO types (name, class) SELECT 'any', id FROM type_classes WHERE name = 'union';"))
    
(define create-union-type-table-query
  "CREATE TABLE union_types (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL,
    member_type INTEGER REFERENCES types(id)
  );")

(define populate-union-type-table-queries
  '("INSERT INTO union_types (name, member_type) SELECT 'any', id FROM types WHERE types.name = 'integer';" 
    "INSERT INTO union_types (name, member_type) SELECT 'any', id FROM types WHERE types.name = 'float';"
    "INSERT INTO union_types (name, member_type) SELECT 'any', id FROM types WHERE types.name = 'boolean';"
    "INSERT INTO union_types (name, member_type) SELECT 'any', id FROM types WHERE types.name = 'string';"
    "INSERT INTO union_types (name, member_type) SELECT 'any', id FROM types WHERE types.name = 'date';"
    "INSERT INTO union_types (name, member_type) SELECT 'any', id FROM types WHERE types.name = 'time';"
    "INSERT INTO union_types (name, member_type) SELECT 'any', id FROM types WHERE types.name = 'datetime';"
    "INSERT INTO union_types (name, member_type) SELECT 'any', id FROM types WHERE types.name = 'nref';"
    "INSERT INTO union_types (name, member_type) SELECT 'any', id FROM types WHERE types.name = 'rref';"
    "INSERT INTO union_types (name, member_type) SELECT 'any', id FROM types WHERE types.name = 'xref';"))

(define create-struct-members-table-query
  "CREATE TABLE struct_type_members (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    struct_type INTEGER REFERENCES struct_types(id), 
    rel_name TEXT NOT NULL,
    cardinality INTEGER REFERENCES cardinalities(id),
    mem_type INTEGER REFERENCES types(id)
  );")

(define create-triple-table-query
  "CREATE TABLE triples (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    s  TEXT NOT NULL,
    st  INTEGER REFERENCES types(id) NOT NULL,
    p  TEXT NOT NULL,
    o  TEXT NOT NULL,
    ot  INTEGER REFERENCES types(id) NOT NULL
  );")

;;; ========================================================================
;;; ------  Functions  -----------------------------------------------------

(define (create-db filename)
  (let* ((db
          (open-database filename))
         (qx
          (lambda (q)
            (let ((s (sql/transient db q)))
              (exec s)))))
    (for-each
      qx
      `(,create-primitive-table-query
        ,create-string-type-table-query
        ,create-number-type-table-query
        ,create-vocab-table-query
        ,create-cardinality-table-query
        ,create-struct-type-table-query
        ,create-type-class-table-query
        ,create-types-table-query
        ,create-union-type-table-query
        ,create-struct-members-table-query
        ,create-triple-table-query))
    (for-each
      (lambda (qlist) (for-each qx qlist))
      `(,populate-primitive-table-queries
        ,populate-cardinality-table-queries
        ,populate-type-class-table-queries
        ,populate-types-table-queries
        ,populate-union-type-table-queries))
    (close-database db)))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ----  USER-DEFINED TYPE MANAGEMENT  ------------------------------------

;;; ------  Queries  -------------------------------------------------------

(define add-string-type-query
  "INSERT INTO string_types (name, pattern) VALUES (?, ?);")

(define add-number-type-query
  "INSERT INTO number_types (name) VALUES (?);")

(define add-number-type-query
  "INSERT INTO number_types (name) VALUES (?);")

(define add-vocab-type-term-query
  "INSERT INTO vocabs (name, term) VALUES (?, ?);")

(define add-struct-type-query
  "INSERT INTO struct_types (name, extensible) VALUES (?, ?);")

(define add-struct-type-member-query
  "INSERT INTO struct_type_members (struct_type, rel_name, cardinality, mem_type)
    SELECT struct_types.id, ?, cardinalities.id, types.id)
    FROM struct_types, cardinalities, types
    WHERE struct_types.name = ?  AND cardinalities.name = ? AND types.name = ?;"

(define add-union-type-member-query
  "INSERT INTO union_types (name, member_type) VALUES (?, ?);")

(define add-type-query
  "INSERT INTO types (name, class) VALUES (?, ?);")

(define update-string-type-query
  "UPDATE string_types SET pattern = ? WHERE name = ?;")

(define update-number-type-min-query
  "UPDATE number_types SET minval = ? WHERE name = ?;")

(define update-number-type-max-query
  "UPDATE number_types SET maxval = ? WHERE name = ?;")

(define update-number-type-step-query
  "UPDATE number_types SET step = ? WHERE name = ?;")

(define update-number-type-digits-query
  "UPDATE number_types SET digits = ? WHERE name = ?;")

(define update-vocab-type-delete-term-query
  "DELETE FROM vocabs WHERE name = ? and term = ?;")

(define update-struct-type-query "")

(define update-union-type-delete-member-query
  "DELETE FROM union_types WHERE name = ? and member_type = ?;")

(define delete-string-type-query
  "DELETE FROM string_types WHERE name = ?;")

(define delete-number-type-query
  "DELETE FROM number_types WHERE name = ?;")

(define delete-vocab-type-query
  "DELETE FROM vocab_types WHERE name = ?;")

(define delete-struct-type-query
  "DELETE FROM struct_types WHERE name = ?;")

(define delete-union-type-query
  "DELETE FROM union_types WHERE name = ?;")

(define delete-type-query
  "DELETE FROM types WHERE name = ?;")

;;; ========================================================================
;;; ------  Functions  -----------------------------------------------------

(define (add-string-type name pattern)
  #f)

(define (add-number-type name #!key (minval #f) (maxval #f) (step #f) (digits #f))
  #f)

(define (add-vocab-type name terms)
  #f)

(define (add-struct-type name #!key (extensible #t) (members '()))
  #f)

(define (add-union-type name members)
  #f)

(define (update-string-type name pattern)
  #f)

(define (update-number-type name #!key (minval #f) (maxval #f) (step #f) (digits #f))
  #f)

(define (update-vocab-type name terms)
  #f)

(define (update-struct-type name #!key (extensible #t) (members '()))
  #f)

(define (update-union-type name members)
  #f)

(define (delete-string-type name pattern)
  #f)

(define (delete-number-type name #!key (minval #f) (maxval #f) (step #f) (digits #f))
  #f)

(define (delete-vocab-type name terms)
  #f)

(define (delete-struct-type name #!key (extensible #t) (members '()))
  #f)

(define (delete-union-type name members)
  #f)

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

) ; END MODULE

;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ------------------------------------------------------------------------

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

;;; ========================================================================
;;; ------------------------------------------------------------------------

