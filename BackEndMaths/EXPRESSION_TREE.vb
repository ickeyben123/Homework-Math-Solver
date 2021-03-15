Class EXPRESSION_TREE : Inherits POSTFIX_EXPRESSION ' This class builds upon the POSTFIX_EXPRESSION and enables the creation of an expression tree. This class is to be used instead of POSTFIX_EXPRESSION when that feature is required.
    Dim RESULT_STACK As Stack(Of TREE_NODE) = New Stack(Of TREE_NODE)
    Public TREE_ROOT As TREE_NODE

    Public Sub CREATE_TREE()
        Dim ROOT As New TREE_NODE
        For Each LETTER As String In OUTPUT_POSTFIX ' Loops through the converted expression list.
            Console.WriteLine("ITEMN" & LETTER)
            If Not OPERATORS.ContainsKey(LETTER) Then ' This means it is an operand, such as 30.
                ROOT = New TREE_NODE ' Creates a node for the operand
                ROOT.VALUE = LETTER
                RESULT_STACK.Push(ROOT) ' Pushes it onto the stack
            Else ' It is an operator, like +
                ROOT = New TREE_NODE ' New tree node
                ROOT.VALUE = LETTER ' Makes the operator the value
                ROOT.RIGHT.Add(RESULT_STACK.Pop) ' Remember that the right and left are lists of nodes. This is important later on. 
                ROOT.LEFT.Add(RESULT_STACK.Pop) ' Pops the stack twice for the correct operators.
                RESULT_STACK.Push(ROOT) ' Pushes the resultant node into the stack.
            End If
        Next
        TREE_ROOT = RESULT_STACK.Pop() ' This will be the root of the tree created.
    End Sub

    Sub New(Input As String)
        MyBase.New(Input)
    End Sub
End Class

