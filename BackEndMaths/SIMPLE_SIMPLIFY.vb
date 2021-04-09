
Imports System.Text.RegularExpressions
' This is a simplification class, which works out the solutions to problems like 300x+50x+2y-4z
' This class inherits EXPRESSION_TREE, so that it can create a tree to parse the solution easily.
' This class was made only to solve + and - expressions. 
' More advanced versions will inherit this class, and override the recursive solver.
' Redunancy may appear.

Class SIMPLE_SIMPLIFY : Inherits EXPRESSION_TREE

    Public RESULT As String
    Public OPTIMISER_CLASS As New OPTIMISER

    Sub New(INPUT As String)
        MyBase.New(INPUT)
        CREATE_TREE() ' Creates a tree from the input specified.
        RESULT = OPTIMISER_CLASS.OPTIMISE_TREE(TREE_ROOT)
    End Sub

    Public Sub DIFFERENTIATE() 'Intermediary for the recursive solver.
        RESULT = OPTIMISER_CLASS.DIFFERENTIATE()
    End Sub
    Public Sub EXPAND() 'Intermediary for the recursive solver.
        RESULT = OPTIMISER_CLASS.EXPAND_BRACKETS()
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


End Class