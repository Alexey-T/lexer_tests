<abbr class="abbr"></abbr>

<!-- Single statement block  --> 
@Code dim myMessage = "Hello World" End Code
 
<!-- Inline expression or variable --> 
<p>The value of myMessage is: @myMessage</p> 
 
<!-- Multi-statement block -->
@Code
dim greeting = "Welcome to our site!" 
dim weekDay = DateTime.Now.DayOfWeek 
dim greetingMessage = greeting & " Today is: " & weekDay
End Code

<p>The greeting is: @greetingMessage</p>