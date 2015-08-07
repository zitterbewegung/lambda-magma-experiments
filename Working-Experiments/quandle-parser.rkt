#lang ragg
expr: element* 
element: LEFT-PAREN element element RIGHT-PAREN element |  LEFT-PAREN element element RIGHT-PAREN LEFT-PAREN element element RIGHT-PAREN