Introduction
===============

This is KeystrokeDynamics.R, a tool for analyzing keystroke dynamics in Web application and Software .

Keystroke dynamics or typing dynamics refers to the automated method of identifying or confirming the identity of an individual based on the manner and the rhythm of typing on a keyboard. Keystroke dynamics is a behavioral biometric, this means that the biometric factor is 'something you do'.

Already during the second world war a technique known as The Fist of the Sender was used by military intelligence to distinguish based on the rhythm whether a morse code message was send by ally or enemy. These days each household has at least one computer keyboard, making keystroke dynamics the easiest biometric solution to implement in terms of hardware.

##How it works
With keystroke dynamics the biometric template used to identify an individual is based on the typing pattern, the rhythm and the speed of typing on a keyboard. The raw measurements used for keystroke dynamics are dwell time and flight time.

Dwell time is the time duration that a key is pressed
Flight time is the time duration in between releasing a key and pressing the next key
When typing a series of characters, the time the subject needs to find the right key (flight time) and the time he holds down a key (dwell time) is specific to that subject, and can be calculated in such a way that it is independent of overall typing speed. The rhythm with which some sequences of characters are typed can be very person dependent. For example someone used to typing in english will be quicker at typing certain character sequences such as 'the' than a person with french roots.


Project file Markdown
======================
####Compare.R 
contains function comparerythm() which take Down Down time give nearness between 0 to 1

####Classifire.R
contains all the classifire and distance matrics .

####readFrom.R
conatains function readFrom() which read txt or csv file from url

####gettingAndCleaning.R
clean data according to algorithm

####Data.R
contains all the required library and input urls

Compatibility notes
======================
Our R script uses the packges "nnclust" , "MASS", "kernlab"
-supported environment window,linux

Refference
=================
http://www.biometric-solutions.com/

http://www.cs.cmu.edu/~keystroke/
