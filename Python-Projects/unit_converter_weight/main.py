# -*- coding: utf-8 -*-
"""
Project Name: Unit Converter for Weight 
Author: Ugur Uresin
Github: ugururesin
"""

## Welcoming Message
print("Welcome to the Weight Unit Converter!")

## Libraries
from tkinter import *

## Creating a GUI Window
window = Tk()

def from_kg():
    
    # gram2kg
    gram = float(e2_value.get()) * 1000
    
    # kg2pound
    pound = float(e2_value.get()) * 2.20462
    
    # kg2ounce
    ounce = float(e2_value.get()) * 35.274
    
    # enters the converted weight to the text widget
    t1.delete("1.0", END)
    t1.insert(END, gram)
    
    t2.delete("1.0", END)
    t2.insert(END, pound)
    
    t3.delete("1.0", END)
    t3.insert(END, ounce)

# creating the label widgets
e1 = Label(window, text= "Enter the weight in  Kg")
e2_value = StringVar()
e2 = Entry(window, textvariable = e2_value)
e3 = Label(window, text="Gram")
e4 = Label(window, text="Pounds")
e5 = Label(window, text="Ounce")
   
# creating the text widgets
t1 = Text(window, height=1, width=20)
t2 = Text(window, height=1, width=20)
t3 = Text(window, height=1, width=20)
   
# creating the button widget
b1 = Button(window, text="Convert", command=from_kg)

# grid method to place the widgets at respective positions in table like str
e1.grid(row=0, column=0)
e2.grid(row=0, column=1)
e3.grid(row=1, column=0)
e4.grid(row=1, column=1)
e5.grid(row=1, column=2)
t1.grid(row=2, column=0)
t2.grid(row=2, column=1)
t3.grid(row=2, column=2)
b1.grid(row=0, column=2)

# starting the gui
window.mainloop()        