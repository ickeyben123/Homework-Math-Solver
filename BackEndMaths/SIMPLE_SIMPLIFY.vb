
Imports System.Text.RegularExpressions
' This is a simplification class, which works out the solutions to problems like 300x+50x+2y-4z
' This class inherits EXPRESSION_TREE, so that it can create a tree to parse the solution easily.
' This class was made only to solve + and - expressions. 
' More advanced versions will inherit this class, and override the recursive solver.
' Redunancy may appear.

Class SIMPLE_SIMPLIFY : Inherits EXPRESSION_TREE

    Public RESULT As String

    Sub New(INPUT As String)
        MyBase.New(INPUT)
        CREATE_TREE() ' Creates a tree from the input specified.
        Dim OPTIMISER As New OPTIMISER
        OPTIMISER.OPTIMISE_TREE(TREE_ROOT)
    End Sub


    Public Sub SIMPLIFY() 'Intermediary for the recursive solver.
        ' RESULT = RECURSIVE_SOLVER(TREE_ROOT)
    End Sub

    Private Function ZERO_TO_NEGATIVE(NUMBER As Integer)
        If NUMBER = 0 Then
            Return -1
        End If
        Return NUMBER
    End Function

    Private Function REMOVE_ZERO_CLEANUP(INPUT As String) ' This is used to cleanup strings that contain 0 elements. This is required due to the nature of the solver. 
        Console.WriteLine("TO DODD" & INPUT)
        Dim ELEMENTED_INPUT = INPUT.ToCharArray ' The array {-,6,x,^,2,+}
        Dim TO_ADD As New Queue(Of String) ' This will be used to create 'items', like 3x^2. This is then added to a final list. 
        Dim PREVIOUS_ELEMENT As String = ""
        Dim ELEMENT_TO_ADD As String = ""
        Dim FINISHED_LIST As New List(Of String)
        For Each ELEMENT As String In ELEMENTED_INPUT
            Console.WriteLine("Current Element" & ELEMENT)
            If PREVIOUS_ELEMENT <> "+" And PREVIOUS_ELEMENT <> "-" And PREVIOUS_ELEMENT <> "*" And PREVIOUS_ELEMENT <> "/" Then ' The previous element isn't an operator.
                FINISHED_LIST.Add(ELEMENT) ' Add the operator seperately.
            Else
                If TO_ADD.Count > 0 Then
                    For I As Integer = 1 To TO_ADD.Count()
                        ELEMENT_TO_ADD = ELEMENT_TO_ADD & TO_ADD.Dequeue()
                    Next
                    FINISHED_LIST.Add(ELEMENT_TO_ADD)
                    ELEMENT_TO_ADD = ""
                End If
                TO_ADD.Enqueue(ELEMENT)
                FINISHED_LIST.Add(PREVIOUS_ELEMENT) ' Add actual data.
            End If
            PREVIOUS_ELEMENT = ELEMENT
        Next
        For I As Integer = 1 To TO_ADD.Count()
            ELEMENT_TO_ADD = ELEMENT_TO_ADD & TO_ADD.Dequeue()
        Next
        FINISHED_LIST.Add(ELEMENT_TO_ADD)
        ELEMENT_TO_ADD = ""
        For Each ELEMENT As String In FINISHED_LIST
            Console.WriteLine("FIISHED " & ELEMENT)
        Next
        Return True

    End Function

    'Private Function RECURSIVE_SOLVER(NODE As TREE_NODE) ' This is used to simplify moderate expressions.
    '    ' Current ability, + & - 
    '    ' TODO MULTIPLICATION!
    '    If NODE Is Nothing Then ' If supplied with a nil node
    '        Return 0
    '    End If

    '    If NODE.LEFT Is Nothing And NODE.RIGHT Is Nothing Then ' Returns the nodes value when nothing is able to be done.
    '        Return NODE.VALUE
    '    End If

    '    Dim LEFT_SOLUTION As String = RECURSIVE_SOLVER(NODE.LEFT) ' Recursively solves the tree
    '    Dim RIGHT_SOLUTION As String = RECURSIVE_SOLVER(NODE.RIGHT)
    '    Dim RESULTANT_ANSWER As List(Of String) = New List(Of String) ' This will hold the answer to be returned.

    '    Dim LEFT_ELEMENTS As Dictionary(Of String, String) = MATCH_COLLECTION_TO_DICTIONARY(Regex.Matches(LEFT_SOLUTION, "[-,+]*[0-9]*(([a-z^]+[1-9])|([a-z]))*")) ' Gets all the elements, assumes that it isn't in bad form, such as (6x+3x)/(5x-2x)
    '    Dim RIGHT_ELEMENTS As Dictionary(Of String, String) = MATCH_COLLECTION_TO_DICTIONARY(Regex.Matches(RIGHT_SOLUTION, "[-,+]*[0-9]*(([a-z^]+[1-9])|([a-z]))*")) ' This includes int
    '    Dim ELEMENT_KEYS As List(Of String) = DICTIONARY_KEYS(LEFT_ELEMENTS)
    '    Dim SUB_ELEMENT_KEYS As List(Of String) = DICTIONARY_KEYS(RIGHT_ELEMENTS)
    '    Dim CHANGE_DICTIONARY As Dictionary(Of String, String) = New Dictionary(Of String, String)
    '    If NODE.VALUE = "+" Or NODE.VALUE = "-" Or NODE.VALUE = "*" Then
    '        ' There can be a difficulty in evaluating this type of thing. 
    '        ' say for example 300x+(20z-30x), where 300x is and operand and (20z-30x) is the current result. 
    '        ' The solution to this would be to loop through each term, ascertaining whether they are negative of positive, and if the two coefficients are the same, we simplify.


    '        For Each Element As KeyValuePair(Of String, String) In LEFT_ELEMENTS
    '            Console.WriteLine("START" & Element.Value)
    '        Next

    '        ' Make every node 'positive'.

    '        If NODE.VALUE = "-" Then '
    '            For Each SUB_TOTAL_ELEMENT_KEY As String In SUB_ELEMENT_KEYS ' I loop through the elements to invert the sign (so I don't have to deal with negative nodes and their incurring strangeness)
    '                Dim OVERALL_NEGATIVITY_COLLECTION_SUB As MatchCollection = Regex.Matches(SUB_TOTAL_ELEMENT_KEY, "[-]") ' Gets the negativity of the other element.
    '                Dim SUM_NEGATIVITY As Integer = 0 ' This is used to determine if I am also minusing from the root node.
    '                Dim OVERALL_NEGATIVITY_SUB As Integer = ZERO_TO_NEGATIVE((OVERALL_NEGATIVITY_COLLECTION_SUB.Count + 1) Mod 2)
    '                Dim SUB_COEFFICIENT As String = MATCH_COLLECTION_TO_STRING(Regex.Matches(SUB_TOTAL_ELEMENT_KEY, "[0-9.]*(?=[a-z])")) ' Gets the coefficient
    '                Dim TOTAL As Integer = CDec(SUB_COEFFICIENT) * (OVERALL_NEGATIVITY_SUB * -1) ' This creates the inverted coefficient 
    '                Dim TO_ADD As String = ""
    '                If TOTAL > 0 Then ' If the coefficient is instead positive I add a + to it. This is important when parsing, otherwise it is typical to encounter 3x4y, without a middle operator.
    '                    TO_ADD = "+"
    '                End If
    '                Dim COMPLETED_ANSWER As String = TO_ADD & TOTAL & MATCH_COLLECTION_TO_STRING(Regex.Matches(RIGHT_ELEMENTS(SUB_TOTAL_ELEMENT_KEY), "([a-z^]+[1-9])|([a-z]*)"))
    '                RIGHT_ELEMENTS.Item(SUB_TOTAL_ELEMENT_KEY) = COMPLETED_ANSWER ' This changes the right element. Remember that I am looping through a list of keys, so I can do this.
    '            Next
    '            NODE.VALUE = "+"
    '        End If
    '        For Each TOTAL_ELEMENT_KEY As String In ELEMENT_KEYS

    '            If LEFT_ELEMENTS(TOTAL_ELEMENT_KEY) <> Nothing Then
    '                Dim TRUE_VALUE As String = LEFT_ELEMENTS(TOTAL_ELEMENT_KEY)
    '                Dim OVERALL_NEGATIVITY_COLLECTION As MatchCollection = Regex.Matches(TRUE_VALUE, "[-]")
    '                Dim OVERALL_VARIABLE As Dictionary(Of String, String) = MATCH_COLLECTION_TO_DICTIONARY(Regex.Matches(TRUE_VALUE, "[a-z]+\^+[0-9.]+")) ' Gets all the variables
    '                If OVERALL_VARIABLE.Count = 0 Then ' This means that it is an integer of some kind. Like 3.
    '                    OVERALL_VARIABLE.Add("INTEGER", "INTEGER")
    '                End If
    '                Dim OVERALL_NEGATIVITY As Integer = ZERO_TO_NEGATIVE(OVERALL_NEGATIVITY_COLLECTION.Count Mod 2) ' if it is 1 then that means it is negative, if it is 0 then I will make it -1.

    '                For Each SUB_TOTAL_ELEMENT_KEY As String In SUB_ELEMENT_KEYS
    '                    If RIGHT_ELEMENTS(SUB_TOTAL_ELEMENT_KEY) <> Nothing Then
    '                        Dim TRUE_VALUE_SUB = RIGHT_ELEMENTS(SUB_TOTAL_ELEMENT_KEY)
    '                        Dim OVERALL_NEGATIVITY_COLLECTION_SUB As MatchCollection = Regex.Matches(TRUE_VALUE_SUB, "[-]") ' Gets the negativity of the other element.
    '                        Dim OVERALL_NEGATIVITY_SUB As Integer = ZERO_TO_NEGATIVE((OVERALL_NEGATIVITY_COLLECTION_SUB.Count) Mod 2) ' if it is 1 then that means it is negative
    '                        Dim OVERALL_VARIABLE_SUB As Dictionary(Of String, String) = MATCH_COLLECTION_TO_DICTIONARY(Regex.Matches(TRUE_VALUE_SUB, "[a-z]+\^+[0-9.]+")) ' Gets all the variables and converts it to a dictionary. 
    '                        ' -4xy - -5xy
    '                        For Each ELEMENT As KeyValuePair(Of String, String) In OVERALL_VARIABLE_SUB ' Looping through the Sub variable
    '                            If OVERALL_VARIABLE.ContainsKey(ELEMENT.Key) <> Nothing Then ' If the dictionary we are comparing to has a variable, say xyz, and xx, then if it finds an x it will remove it from what we are comparing to.
    '                                OVERALL_VARIABLE.Remove(ELEMENT.Key) ' If the compared dictionary is empty by the end, it means that they can be added .
    '                            End If
    '                        Next
    '                        If OVERALL_VARIABLE.Count = 0 Or (OVERALL_VARIABLE.ContainsKey("INTEGER") And OVERALL_VARIABLE_SUB.Count = 0) And NODE.VALUE <> "*" Then ' This means they have the same variables.
    '                            ' I know that it is something + something. And their variabels are the same.
    '                            ' I have worked out whether each are negative or not, so I can now work out the sum. 
    '                            Dim COEFFICIENT, SUB_COEFFICIENT As String
    '                            If OVERALL_VARIABLE.ContainsKey("INTEGER") Then
    '                                COEFFICIENT = MATCH_COLLECTION_TO_STRING(Regex.Matches(TRUE_VALUE, "[0-9.]*")) ' Gets the coefficient
    '                                SUB_COEFFICIENT = MATCH_COLLECTION_TO_STRING(Regex.Matches(TRUE_VALUE_SUB, "[0-9.]*")) ' Gets the coefficient
    '                            Else
    '                                COEFFICIENT = MATCH_COLLECTION_TO_STRING(Regex.Matches(TRUE_VALUE, "[0-9.]*(?=[a-z])")) ' Gets the coefficient
    '                                SUB_COEFFICIENT = MATCH_COLLECTION_TO_STRING(Regex.Matches(TRUE_VALUE_SUB, "[0-9.]*(?=[a-z])")) ' Gets the coefficient
    '                            End If
    '                            Dim TOTAL As Double = CDec(COEFFICIENT) * (OVERALL_NEGATIVITY * -1) + CDec(SUB_COEFFICIENT) * (OVERALL_NEGATIVITY_SUB * -1)
    '                            Console.WriteLine("TOTAL" & TOTAL & "TRUE" & TRUE_VALUE & "LOL" & MATCH_COLLECTION_TO_STRING(Regex.Matches(TRUE_VALUE, "[a-z]+\^+[0-9.]+")))
    '                            Dim COMPLETED_ANSWER As String = TOTAL & MATCH_COLLECTION_TO_STRING(Regex.Matches(TRUE_VALUE, "[a-z]+\^+[0-9.]+"))
    '                            RIGHT_ELEMENTS.Item(SUB_TOTAL_ELEMENT_KEY) = "" ' Remove the two things I summed
    '                            If Not (COMPLETED_ANSWER.Contains("-")) Then
    '                                COMPLETED_ANSWER = "+" & COMPLETED_ANSWER
    '                            End If
    '                            Console.WriteLine("completed" & COMPLETED_ANSWER)
    '                            LEFT_ELEMENTS.Item(TOTAL_ELEMENT_KEY) = COMPLETED_ANSWER 'This means I can know where to put my sum.
    '                            Exit For
    '                        ElseIf NODE.VALUE = "*" Then ' This implements the multiplication standard. 

    '                        End If
    '                    End If
    '                Next

    '            End If
    '        Next

    '        ' ELEMENT +2X, SHOULDN'T INCLUDE A +, CAUSES 11X5, WITHOUT A + IN THE MIDDLE.

    '        Dim TO_MODIFY As String = ""
    '        Console.WriteLine("now to be selected" & DICTIONARY_TO_STRING(LEFT_ELEMENTS) & NODE.VALUE & DICTIONARY_TO_STRING(RIGHT_ELEMENTS))
    '        Dim COLLECTION As List(Of String) = REMOVE_ZERO_CLEANUP(DICTIONARY_TO_STRING(LEFT_ELEMENTS) & NODE.VALUE & DICTIONARY_TO_STRING(RIGHT_ELEMENTS))
    '        ' Dim COLLECTION = Regex.Matches(DICTIONARY_TO_STRING(LEFT_ELEMENTS) & NODE.VALUE & DICTIONARY_TO_STRING(RIGHT_ELEMENTS), "my ks l(\+|-)*(([1-9]+[0]*[a-z]*([a-z]|[0-9]))|([1-9]))") ' This regex only gets the non zero entities in the expression
    '        Dim COUNT As Integer = 0

    '        'For Each ELEMENT As String In COLLECTION
    '        ' Dim NUM_NEGATIVES As Integer = Regex.Matches(ELEMENT, "[-]").Count ' Gets the number of negative signs
    '        'Dim NUM_POSITIVES As Integer = Regex.Matches(ELEMENT, "[+]").Count ' Number of positive signs
    '        ' Dim TO_ADD As String = ""
    '        '  If NUM_NEGATIVES Mod 2 = 0 And NUM_POSITIVES > 0 And COUNT > 0 Then ' This does some logic to see what to add to the end. This is only useful when there are multiple signs, such as -+ +- ++, etc. This will simplify any extremities that may arise through my parsing.
    '        'TO_ADD = "+"
    '        'ElseIf NUM_NEGATIVES Mod 2 = 1 Then
    '        '     TO_ADD = "-"
    '        '  End If
    '        'TO_MODIFY = TO_MODIFY & TO_ADD & MATCH_COLLECTION_TO_STRING(Regex.Matches(ELEMENT, "[0-9]*[a-z]*")) ' Creates a final product. Something like - & 3x
    '        '      COUNT += 1
    '        '      Next
    '        '      Return TO_MODIFY


    '    End If
    '    Return NODE.LEFT.VALUE & NODE.VALUE & NODE.RIGHT.VALUE
    'End Function

End Class