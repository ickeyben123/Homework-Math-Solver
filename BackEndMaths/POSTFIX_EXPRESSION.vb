Imports System.Text.RegularExpressions
'\(([^(]+)\) FINDS ANY ITEMS WITH (..)
Class POSTFIX_EXPRESSION : Inherits UTILITIES ' defines any expression that is wished to be simplified to postfix.

    Protected OPERATORS As Dictionary(Of String, Array) = New Dictionary(Of String, Array) From
    {{"+", {2, False}}, {"-", {2, False}}, {"/", {3, False}}, {"*", {3, False}}, {"^", {4, True}}} 'defines precedence 

    Protected TO_BE_SIMPLFIED As String ' original input string
    Protected STRING_ARRAY() As Char ' an array of the string for each character
    Protected INFIX_VERSION As String ' the infix version of the string, 3+3 would be 3 3 +
    Protected OUTPUT_QUEUE As Queue(Of String) = New Queue(Of String)() ' used to convert infix to postfix
    Protected OPERATOR_STACK As Stack(Of String) = New Stack(Of String)()
    Protected OUTPUT_POSTFIX As List(Of String) = New List(Of String)
    Protected TRUE_EXPRESSION_LIST As List(Of String) = New List(Of String)

    'FIXED! 
    Function CONVERT_CHAR_ARRAY_TO_CORRECT_FORM(INPUT As String) ' This changes something like {1,3,x,+,3} into {13x,+,3}, otherwise I wouldn't be able to tell where the numbers end, easily.
        Dim TEMP_QUEUE As Queue(Of String) = New Queue(Of String) ' A temporary queue used to hold the numbers, say if I have {1,3,x,+}, it will add {1,3,x} to the stack, and stop at the +. 
        Dim TEMP_STRING_LIST As List(Of String) = New List(Of String)
        Dim CONTINUE_ADD_UNTIL_OPERATOR As Boolean = False ' This is used whenever something lke a 3x^23 occurs. When it sees a ^ it will activate, and continue until a operator is seen.
        Dim COUNT As Integer
        For Each CHAR_TO_COMBINE As String In INPUT
            COUNT += 1
            If (CONTINUE_ADD_UNTIL_OPERATOR And (IsNumeric(CHAR_TO_COMBINE) Or Char.IsLetter(CHAR_TO_COMBINE))) Or (IsNumeric(CHAR_TO_COMBINE) Or Char.IsLetter(CHAR_TO_COMBINE)) And Not OPERATORS.ContainsKey(CHAR_TO_COMBINE) And (CHAR_TO_COMBINE <> "(" Or CHAR_TO_COMBINE <> ")") Then
                ' Console.WriteLine(CHAR_TO_COMBINE & " wtf")
                If Not IsNumeric(CHAR_TO_COMBINE) Then ' Adds a multplication sign so that it can be converted to tree form
                    If TEMP_QUEUE.Count > 0 Then ' If the queue contains anything then it will append the letters together
                        TEMP_STRING_LIST.Add(QUEUE_TO_STRING(TEMP_QUEUE))
                        TEMP_QUEUE = New Queue(Of String) ' Resets the stack.
                    End If
                    If TEMP_STRING_LIST.Count >= 1 Then
                        If TEMP_STRING_LIST(TEMP_STRING_LIST.Count - 1) <> "*" And TEMP_STRING_LIST(TEMP_STRING_LIST.Count - 1) <> "(" And TEMP_STRING_LIST(TEMP_STRING_LIST.Count - 1) <> "+" And TEMP_STRING_LIST(TEMP_STRING_LIST.Count - 1) <> "-" And TEMP_STRING_LIST(TEMP_STRING_LIST.Count - 1) <> "/" Then
                            TEMP_STRING_LIST.Add("*")
                            TEMP_STRING_LIST.Add(CHAR_TO_COMBINE) ' Add any numbers or variables to the queue, to be    made as a single entity.
                        Else
                            TEMP_QUEUE.Enqueue(CHAR_TO_COMBINE)
                        End If
                    Else
                        TEMP_QUEUE.Enqueue(CHAR_TO_COMBINE)
                    End If
                Else
                    TEMP_QUEUE.Enqueue(CHAR_TO_COMBINE) ' Add any numbers or variables to the queue, to be made as a single entity.
                End If
            ElseIf CHAR_TO_COMBINE <> " " Then ' Then it is an operator.
                If TEMP_QUEUE.Count > 0 Then ' If the queue contains anything then it will append the letters together
                    TEMP_STRING_LIST.Add(QUEUE_TO_STRING(TEMP_QUEUE)) ' Adds the created string, such as 13x.
                    TEMP_QUEUE = New Queue(Of String) ' Resets the stack.
                    TEMP_STRING_LIST.Add(CHAR_TO_COMBINE)
                Else
                    If CHAR_TO_COMBINE = "-" Then
                        If TEMP_STRING_LIST.Count > 0 Then
                            Dim CHECK As Dictionary(Of String, String) = MATCH_COLLECTION_TO_DICTIONARY(Regex.Matches(TEMP_STRING_LIST(TEMP_STRING_LIST.Count - 1), "[*,+,/,-,(,)]"))
                            If IsNumeric(TEMP_STRING_LIST(TEMP_STRING_LIST.Count - 1)) Or CHECK.Count = 0 Then
                                TEMP_STRING_LIST.Add(CHAR_TO_COMBINE)
                            Else
                                TEMP_QUEUE.Enqueue(CHAR_TO_COMBINE)
                            End If
                        Else
                            TEMP_STRING_LIST.Add("*")
                            TEMP_STRING_LIST.Add(CHAR_TO_COMBINE & "1")
                        End If
                    Else
                        TEMP_STRING_LIST.Add(CHAR_TO_COMBINE)
                    End If
                End If
            End If
        Next
        If TEMP_QUEUE.Count > 0 Then
            TEMP_STRING_LIST.Add(QUEUE_TO_STRING(TEMP_QUEUE)) ' Adds the created string, such as 13x.
        End If
        For Each ELEMENT As String In TEMP_STRING_LIST
            'Console.WriteLine("FINISH2" & ELEMENT)
        Next
        Return TEMP_STRING_LIST ' Sets the global variable.
    End Function

    Sub New(ByVal INPUT As String) ' intitlises systems so that it can be used by the program.
        TO_BE_SIMPLFIED = INPUT ' sets the protected variable to the input specified by the program 
        STRING_ARRAY = INPUT.ToCharArray ' splits the string into an array so that it can be looped through
        Dim COUNT As Integer = 0
        TRUE_EXPRESSION_LIST = CONVERT_CHAR_ARRAY_TO_CORRECT_FORM(STRING_ARRAY)
        For Each ELEMENT As String In TRUE_EXPRESSION_LIST
            'Console.WriteLine("FINISH" & ELEMENT)
        Next
        INFIX_TO_POSTFIX() ' CONVERTS POSTFIX TO INFIX

    End Sub

    ' the output queue contains the output of the conversion
    ' the stack is used for operators
    ' the algorithm used here is called the 'shunting yard algorithm',  first described by dijkstra.
    ' the method and actual conversion will be modified to accomodate for actual variables, such as 5x + 2x, not just 2 + 3; otherwise it would be pointless.

    Public Sub INFIX_TO_POSTFIX() ' converts the infix input to postfix (reverse polish notation).
        For I = 0 To TRUE_EXPRESSION_LIST.Count() - 1
            Dim CURRENT_CHAR As String = TRUE_EXPRESSION_LIST(I)
            If CURRENT_CHAR = "(" Then ' it pushes the ( onto the operator statck to begin parenthese
                OPERATOR_STACK.Push(CURRENT_CHAR)
            ElseIf CURRENT_CHAR = ")" Then ' this means it has closed parenthese, so it will add the operators from the operator stack onto the output queue
                While OPERATOR_STACK.Peek() <> "("
                    OUTPUT_QUEUE.Enqueue(OPERATOR_STACK.Peek())
                    OPERATOR_STACK.Pop()
                End While
                OPERATOR_STACK.Pop() ' pops the )
            ElseIf OPERATORS.ContainsKey(CURRENT_CHAR) Then ' if it is an operator
                Dim OPERATOR_DATA As Array = OPERATORS.Item(CURRENT_CHAR)
                If OPERATOR_STACK.Count > 0 Then
                    If OPERATOR_STACK.Peek() <> "(" Then ' this is placed outside due to the operators dictionary not containing "(", thus the while loop would error out.
                        While OPERATORS.Item(OPERATOR_STACK.Peek())(0) > OPERATOR_DATA(0) Or
                            (OPERATORS.Item(OPERATOR_STACK.Peek())(0) = OPERATOR_DATA(0) And OPERATOR_DATA(1) = False) ' this will pop the stack with any operations that cannot continue to be held when this operator is pushed onto the stack
                            ' the workings include, if the current top stack operator has a higher precedence or if the operator has the same precedence and the token (the one being added) is left (false) in associativity
                            ' the stack item must also not be a ')'
                            OUTPUT_QUEUE.Enqueue(OPERATOR_STACK.Peek())
                            OPERATOR_STACK.Pop()
                            If OPERATOR_STACK.Count = 0 Then
                                Exit While
                            End If
                            If OPERATOR_STACK.Count > 0 Then
                                If OPERATOR_STACK.Peek() = "(" Then
                                    Exit While
                                End If
                            End If
                        End While
                    End If
                End If
                If Not (CURRENT_CHAR = "-" And TRUE_EXPRESSION_LIST(Math.Abs(I)) = "(") Then
                    OPERATOR_STACK.Push(CURRENT_CHAR) ' pushes the operator after all checks are done
                Else
                    OUTPUT_QUEUE.Enqueue(CURRENT_CHAR)
                End If
            Else ' It is a normal number.
                OUTPUT_QUEUE.Enqueue(CURRENT_CHAR)
            End If
        Next
        While OPERATOR_STACK.Count() > 0 ' while there are items in the operator stack.
            OUTPUT_QUEUE.Enqueue(OPERATOR_STACK.Peek())
            OPERATOR_STACK.Pop()
        End While

        ' output
        Dim COUNT As Integer = 0
        Dim COMPLETED As String = ""
        Do
            COMPLETED = COMPLETED & OUTPUT_QUEUE.Peek()
            OUTPUT_POSTFIX.Add(OUTPUT_QUEUE.Dequeue.ToString())
        Loop While (OUTPUT_QUEUE.Count > 0)
    End Sub

End Class

