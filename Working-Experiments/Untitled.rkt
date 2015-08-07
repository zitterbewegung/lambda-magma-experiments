#lang ragg
nested-word-list: VAR
                | LEFT-PAREN nested-word-list* nested-word-list* RIGHT-PAREN
                | LEFT-PAREN LEFT-PAREN nested-word-list* nested-word-list* RIGHT-PAREN nested-word-list* RIGHT-PAREN

