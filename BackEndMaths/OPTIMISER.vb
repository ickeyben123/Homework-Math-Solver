Imports System.Text.RegularExpressions

Enum ETYPE
    LEFT
    RIGHT
End Enum


Class OPTIMISER : Inherits UTILITIES

    ' General class for optimising trees. 
    Private TREE_TO_MODIFY As TREE_NODE
    '' FIX LIKE TERM SUMMING AS IT BREAKS THE POWER ADDITION
    Public Function OPTIMISE_TREE(TO_MODIFY As TREE_NODE, Optional EXPAND_BRACKETS As Boolean = False)
        ' This function is important in simplifying the tree expressions. The disadvantage of trees are such that certain multiplicative expressions can be interpreted in multiple ways. 
        ' Division and Negative nodes are not optimal for a computer to handle. This function/(collection of functions) aims to solve these issues by rearranging nodes into a form that is fundamentally the same.
        ' ... But much easier to interpret for a tree. 

        ' Change the negative nodes to positive nodes, where the negative value is now a multiply node with -1 and the actual value.
        ' Level operators. Operators like + and * can be changed to have many children, instead of the binary two. This is why removing - and simplifying / operators are important.

        ' Added on are actual simplification, such as collecting like terms, etc.

        Dim LAST_TREE As String

        Dim GLOBAL_LAST_TREE As String

        TREE_TO_MODIFY = TO_MODIFY


        For i = 1 To 8
            GLOBAL_LAST_TREE = IN_ORDER(TREE_TO_MODIFY, True)
            PREPARATION_BY_TIMES_RULING(TREE_TO_MODIFY)
            NEGATIVE_POWERS_TO_DIVISORS(TREE_TO_MODIFY)
            TREE_TO_MODIFY = TO_MODIFY
            While Not LAST_TREE = IN_ORDER(TREE_TO_MODIFY, True)
                LAST_TREE = IN_ORDER(TREE_TO_MODIFY, True)
                REMOVE_NEGATIVITY(TREE_TO_MODIFY) ' removes the negative roots.
                TREE_TO_MODIFY = TO_MODIFY
                LEVEL_OPERATORS(TREE_TO_MODIFY) ' levels the operators, see function for more details.
                RATIONAL_SIMPLIFICATION(TREE_TO_MODIFY)
            End While
            LAST_TREE = ""
            While Not LAST_TREE = IN_ORDER(TREE_TO_MODIFY, True)
                LAST_TREE = IN_ORDER(TREE_TO_MODIFY, True)
                PREPARATION_BY_TIMES_RULING(TREE_TO_MODIFY)
                FRACTION_COLLECTER_WRAPPER(TREE_TO_MODIFY)
                COLLECT_LIKE_TERMS(TREE_TO_MODIFY)
            End While
            'Console.WriteLine("finished boi v2")
            If EXPAND_BRACKETS Then
                MULTIPLIER_SIMPLIFIER(TREE_TO_MODIFY)
            End If
            MULTIPLIER_WRAPPER_SOLVER(TREE_TO_MODIFY)
            PREPERATION_POWER_RULING(TREE_TO_MODIFY)
            POWER_SOLVER(TREE_TO_MODIFY)
            'Console.WriteLine("finished boi v2")
            'Console.WriteLine(IN_ORDER(TREE_TO_MODIFY, True))
            REMOVE_LOOSE_ADDITIONS(TREE_TO_MODIFY)
            REMOVE_POWERS(TREE_TO_MODIFY)
            CLEANUP_FINALISER(TREE_TO_MODIFY)
            'REMOVE_DIVISION_ENTIRELY(TREE_TO_MODIFY)
            '  DIVISION_MULTIPLICATION(TREE_TO_MODIFY)
        Next
        ' Console.WriteLine("finished boi v3")
        ' Console.WriteLine(IN_ORDER(TREE_TO_MODIFY, True))
        Return IN_ORDER(TREE_TO_MODIFY, True)
    End Function

    Public Function EXPAND_BRACKETS()
        BRACKET_POWER_EXPANDER(TREE_TO_MODIFY)
        OPTIMISE_TREE(TREE_TO_MODIFY, True)
        Return IN_ORDER(TREE_TO_MODIFY, True)
    End Function

    Public Sub BRACKET_POWER_EXPANDER(NODE As TREE_NODE)
        'u^3=u*u*u

        If Not NODE.LEFT Is Nothing Then
            For Each NODE_ELEMENT As TREE_NODE In NODE.LEFT
                BRACKET_POWER_EXPANDER(NODE_ELEMENT)
            Next
        End If
        If Not NODE.RIGHT Is Nothing Then
            For Each NODE_ELEMENT As TREE_NODE In NODE.RIGHT
                BRACKET_POWER_EXPANDER(NODE_ELEMENT)
            Next
        End If

        If NODE.VALUE = "^" Then
            If IsNumeric(NODE.RIGHT(0).VALUE) Then
                NODE.VALUE = "*"
                For I As Integer = 1 To NODE.RIGHT(0).VALUE - 1
                    If NODE.RIGHT.Count = 1 Then
                        NODE.RIGHT.Add(NODE.LEFT(0).CLONE)
                    Else
                        NODE.LEFT.Add(NODE.LEFT(0).CLONE)
                    End If
                Next
                NODE.RIGHT.RemoveAt(0)
            End If
        End If


    End Sub

    Private OPERATORS = New Dictionary(Of String, Func(Of Double, Double, Double))() From { ' An operator dictionary.
    {"+", Function(x, y) x + y},
    {"-", Function(x, y) x - y},
    {"/", Function(x, y) x / y},
    {"*", Function(x, y) x * y},
    {"^", Function(x, y) x ^ y}
}
    Public Function OUTPUT_VALUE(NODE As TREE_NODE, VALUE As Integer) As Double
        ' This is a simple algorithm that will set every variable to the input parameter 'VALUE'.
        ' The output will then be returned.

        ' This only exists to compare functions easily, as even if two functions are equal, their form can be different.
        ' This will be primarially used in the homework submission area.

        ' The general idea will be to compute the lowest left node, then return this value, and go up to the root, and through the right. 
        ' Ie I calculate the left dependent on the root, then I 'go to the root' and calculate the right node with the computed left node as the starting point.
        ' Normally this would be a binary tree, so I would actually 'go to the root' by calculating the left and right, but as trees can have more than one in the left and right, I must first calculate their stuff.

        ' The values of each node will be calulated much the same, so by virtue of this algorithm I go from the bottom to the top.
        Dim COMPUTED As Double
        If NODE.VALUE = "*" Or NODE.VALUE = "/" Or NODE.VALUE = "^" Then
            COMPUTED = 1 'This will be the starting integer I will go off.
        ElseIf NODE.VALUE = "+" Or NODE.VALUE = "-" Then
            COMPUTED = 0 ' Obviously + nodes will start from 0.
        ElseIf IsNumeric(NODE.VALUE) Then
            COMPUTED = NODE.VALUE
        Else
            ' This is probably a variable, like an x, so I return 1.
            Return VALUE
        End If

        If Not NODE.LEFT Is Nothing Then
            For Each NODE_ELEMENT As TREE_NODE In NODE.LEFT
                Dim OPERATION As Func(Of Double, Double, Double) = OPERATORS(NODE.VALUE)
                COMPUTED = OPERATION(COMPUTED, OUTPUT_VALUE(NODE_ELEMENT, VALUE)) ' This will calculate the resultant.
            Next
        End If

        If Not NODE.RIGHT Is Nothing Then
            For Each NODE_ELEMENT As TREE_NODE In NODE.RIGHT
                Dim OPERATION As Func(Of Double, Double, Double) = OPERATORS(NODE.VALUE) ' The same deal for the right.
                COMPUTED = OPERATION(COMPUTED, OUTPUT_VALUE(NODE_ELEMENT, VALUE))
            Next
        End If

        Return COMPUTED
    End Function

    Public Sub REMOVE_DIVISION_ENTIRELY(NODE As TREE_NODE)
        ' THIS ASSUMES NUMERIC POWERS!!
        If Not NODE.LEFT Is Nothing Then
            For Each NODE_ELEMENT As TREE_NODE In NODE.LEFT
                REMOVE_DIVISION_ENTIRELY(NODE_ELEMENT)
            Next
        End If
        If Not NODE.RIGHT Is Nothing Then
            For Each NODE_ELEMENT As TREE_NODE In NODE.RIGHT
                REMOVE_DIVISION_ENTIRELY(NODE_ELEMENT)
            Next
        End If

        If NODE.VALUE = "/" Then
            Dim POWER_NODE, MINUS_NODE As New TREE_NODE
            MINUS_NODE.VALUE = "-1"
            If NODE.RIGHT(0).VALUE <> "^" Then
                POWER_NODE.VALUE = "^"
                POWER_NODE.RIGHT.Add(MINUS_NODE) ' So the power is ^-1, the same as division
                POWER_NODE.LEFT.AddRange(NODE.RIGHT) ' The actual divisor
                NODE.RIGHT.RemoveRange(0, NODE.RIGHT.Count)
                NODE.RIGHT.Add(POWER_NODE)
            Else
                NODE.RIGHT(0).RIGHT(0).VALUE = "-" & NODE.RIGHT(0).RIGHT(0).VALUE
            End If
            NODE.VALUE = "*"
        End If


    End Sub

    Public Function DIFFERENTIATE(Optional TO_MODIFY As TREE_NODE = Nothing)
        If TO_MODIFY IsNot Nothing Then
            TREE_TO_MODIFY = TO_MODIFY
        End If

        ' This is only here as when there is no + node at the top, things get iffy for some algorithms designed around them.
        ' I could fix it but my attempts just make things annoying.
        If TREE_TO_MODIFY.VALUE <> "+" Then
            Dim ADD_NODE, ZERO_NODE As New TREE_NODE
            ADD_NODE.VALUE = "+"
            ZERO_NODE.VALUE = "0"
            ADD_NODE.LEFT.Add(TREE_TO_MODIFY)
            ADD_NODE.RIGHT.Add(ZERO_NODE)
            TREE_TO_MODIFY = ADD_NODE
        End If
        REMOVE_DIVISION_ENTIRELY(TREE_TO_MODIFY)
        PREPARATION_BY_TIMES_RULING(TREE_TO_MODIFY)
        PREPERATION_POWER_RULING(TREE_TO_MODIFY)
        MULTIPLIER_WRAPPER_SOLVER(TREE_TO_MODIFY)
        DIFFERENTATION_WRAPPER(TREE_TO_MODIFY)
        ' Simplify the differentiated
        ' NEGATIVE_POWERS_TO_DIVISORS(TREE_TO_MODIFY)
        OPTIMISE_TREE(TREE_TO_MODIFY)
        'NEGATIVE_POWERS_TO_DIVISORS(TREE_TO_MODIFY)
        Return IN_ORDER(TREE_TO_MODIFY, True)
    End Function


    ' Simplification Functions
    ' //These are made to actually modify the expression.
    '//// THIS TO BE FINISHED!! ////  TAKE INTO ACCOUNT NEAGTIVE NUMBERS!


    ' // Multiplication Functions

    Enum MULTIPLIER_NUM ' Used by 'MULTIPLIER_SIMPLIFIER' 
        SUM
        OTHER
    End Enum

    Private Function LIMITED_DIFFERENTIATE(NODE As TREE_NODE)
        ' This is a recursive algorithm that differentiates specific input functions.
        ' Differentiation is inherently recursive, or more so it is a downward loop of refining what the derivative actually equals.
        ' Whatever input function I have, I can change it so that it is in proper form.
        ' If it is d(f(x)*f(y)), then the resultant after partial derivatives is d(f(x))*f(y) + f(x)*d(f(y))... or (d(f(x))/dx) *dx + f(x) * d(f(y))/dx * dy
        ' This can only work when I let all functions into some form u, and the basic principles of differentiating powers, or multipliers, follows.

        ' Ie, f(x) and f(y) become f(u) and f(t), where u and t are made to put it in the form (u^a)*(t^b). 

        ' General cases to look for.
        ' When form u^a is found, do the total derivative of a modified function - 
        ' EXAMPLE: f(x) = (9x^2+3)^4, let u = 9x^2+3, so f(u)= u^4, taking total derivative of apparent input -> d(f(u)) = d(f(u))/du * du.
        ' Recursively call the algorithm for "du", so that du is now evaluated, and made to be du/dx, because u is in terms of x and can be further decomposed.

        ' When form (u^a)*(t^b) is found, do product rule derived from partial derivatives,
        ' EXAMPLE: f(x) = ((9x^2+3)^4)*((39x^5+2x)^93), let u = 9x^2+3 and t = 39x^5+2x.  New form f(u,t) = (u^4)*(t^93). So 'multivariable' function.
        ' d(f(u,t)) = sum of partial derivatives = ∂(f(u,t))/∂u *du + ∂(f(u,t))/∂t *dt= d(u^4)*(t^93) + (u^4)*d(t^93), -> For d(u^4), the same principle for recursive differentiation applies,
        ' ie d(u^4) -> d(u^4)/du * du. Call function again for u and differentiate.


        ' This will be an up to down traversal, as this is how differentiating works.
        ' The traversal will only continue when the derivative can be further deconstructed. In the form of u!

        If NODE.VALUE = "*" Then
            ' I will make a general rule for products, even if they are merely 9x^2.
            ' So, it will become a sum of partial derivatives.
            ' If the product has 4 varriables, then there will be 4 sums.
            ' This will be correct as all product derivatives are in this form, but most of the time the partial derivatives end up turning to nothing.
            ' General form, say y= f(u,t,c,...) -> d(f(u,t,c,...)) = ∂y/∂u *du + ∂y/∂t * dt + ∂y/∂c * dc ... to infinity. Wonderful elegance in a derivative :).


            Dim COLLECTIVE = {NODE.LEFT, NODE.RIGHT}
            Dim PLUS_NODE As New TREE_NODE
            PLUS_NODE.VALUE = "+"


            For Each ELEMENT In COLLECTIVE
                For A = 0 To ELEMENT.Count - 1

                    Dim SELECTED_NODE As TREE_NODE = ELEMENT(A)
                    ' Console.WriteLine("the element" & IN_ORDER(ELEMENT(A), True))
                    Dim REFERENCE_NODE As New TREE_NODE
                    REFERENCE_NODE.VALUE = "*"
                    REFERENCE_NODE.LEFT.AddRange(NODE.LEFT)
                    REFERENCE_NODE.RIGHT.AddRange(NODE.RIGHT)

                    If REFERENCE_NODE.LEFT.Contains(ELEMENT(A)) Then
                        REFERENCE_NODE.LEFT.RemoveAt(A)
                    Else
                        REFERENCE_NODE.RIGHT.RemoveAt(A)
                    End If

                    ' This is for the partial part. Ie d(x*y*z) = d(x)*y*z + x*d(y)*z... etc. So I must remove the selected node, and then add the reference on later. 

                    If SELECTED_NODE.VALUE = "^" Then
                        If Not IsNumeric(IN_ORDER(SELECTED_NODE.LEFT(0), True)) Then
                            ' Remember that I assume powers are numeric. I'll deal with logs when I feel succinctly mad.
                            Dim MULTIPLIER As New TREE_NODE
                            MULTIPLIER.VALUE = "*" ' The decomposed derivative
                            Dim NUMERIC_NODE As New TREE_NODE ' the power
                            NUMERIC_NODE.VALUE = SELECTED_NODE.RIGHT(0).VALUE

                            Dim CLONED_SELECTED_NODE As TREE_NODE = SELECTED_NODE.CLONE
                            CLONED_SELECTED_NODE.RIGHT(0).VALUE = CLONED_SELECTED_NODE.RIGHT(0).VALUE - 1 'd(u^3) = 3*u^2 du


                            MULTIPLIER.LEFT.Add(NUMERIC_NODE) ' The power, ie the 3 in the 3*u^2 du
                            MULTIPLIER.RIGHT.Add(CLONED_SELECTED_NODE) ' The remaining function, ie the u^2

                            MULTIPLIER.RIGHT.AddRange(REFERENCE_NODE.LEFT)
                            MULTIPLIER.RIGHT.AddRange(REFERENCE_NODE.RIGHT)
                            Dim NEW_TIMES_NODE, ONE_NODE As New TREE_NODE ' As this algorithm only accepts (x*b), then I will make this *1. Either that or a load of redundancy.
                            ONE_NODE.VALUE = "1"
                            NEW_TIMES_NODE.VALUE = "*"
                            NEW_TIMES_NODE.LEFT.Add(SELECTED_NODE.LEFT(0))
                            NEW_TIMES_NODE.RIGHT.Add(ONE_NODE)

                            If LIMITED_DIFFERENTIATE(NEW_TIMES_NODE) IsNot Nothing Then
                                If (LIMITED_DIFFERENTIATE(NEW_TIMES_NODE).LEFT.COUNT + LIMITED_DIFFERENTIATE(NEW_TIMES_NODE).RIGHT.COUNT) > 0 Then
                                    'Console.WriteLine("brrerrr" & IN_ORDER(LIMITED_DIFFERENTIATE(NEW_TIMES_NODE), True))
                                    MULTIPLIER.RIGHT.Add(LIMITED_DIFFERENTIATE(NEW_TIMES_NODE)) ' the du. or more so du/dx if u is in x.
                                End If
                            End If
                            If PLUS_NODE.LEFT.Count = 0 Then
                                PLUS_NODE.LEFT.Add(MULTIPLIER) ' Add the subsequent derivative to the plus node
                            Else
                                PLUS_NODE.RIGHT.Add(MULTIPLIER) ' Add the subsequent derivative to the plus node
                            End If
                        Else
                            ' This means its an integer ^ of something. so 0.
                            Dim ZERO_NODE As New TREE_NODE
                            ZERO_NODE.VALUE = "0"
                            PLUS_NODE.LEFT.Add(ZERO_NODE)
                        End If

                    ElseIf SELECTED_NODE.VALUE = "+" Then
                        ' Easier to implement.
                        ' Console.WriteLine("im hereeee" & IN_ORDER(SELECTED_NODE, True))
                        Dim SUB_COLLECTIVE = {SELECTED_NODE.LEFT, SELECTED_NODE.RIGHT}
                        Dim NEW_PLUS_NODE As New TREE_NODE
                        NEW_PLUS_NODE.VALUE = "+"
                        For Each SUB_ELEMENT In SUB_COLLECTIVE
                            For B = 0 To SUB_ELEMENT.Count - 1
                                ' Console.WriteLine("LINE THE PLUSSS" & IN_ORDER(SUB_ELEMENT(B), True))
                                Dim DIFFERENTIATED_NODE As TREE_NODE = LIMITED_DIFFERENTIATE(SUB_ELEMENT(B))
                                If NEW_PLUS_NODE.LEFT.Count = 0 Then
                                    NEW_PLUS_NODE.LEFT.Add(DIFFERENTIATED_NODE) ' Ie (x+y+z) = (dx+dy+dz)
                                Else
                                    NEW_PLUS_NODE.RIGHT.Add(DIFFERENTIATED_NODE)
                                End If
                            Next
                        Next
                        ' I now have a differentiated + node. But in context this must be properly formed.
                        Dim NEW_NODE As New TREE_NODE
                        NEW_NODE.VALUE = "*"
                        NEW_NODE.LEFT.Add(NEW_PLUS_NODE)
                        NEW_NODE.RIGHT.AddRange(REFERENCE_NODE.LEFT)
                        NEW_NODE.RIGHT.AddRange(REFERENCE_NODE.RIGHT)
                        PLUS_NODE.LEFT.Add(NEW_NODE)
                    Else
                        Dim CHECK = MATCH_COLLECTION_TO_DICTIONARY(Regex.Matches(SELECTED_NODE.VALUE, "(?=[a-z])[^x]")) ' Regex that looks for a variable that isnt x. Lookahead required to make sure it isn't a number (ie it will only return true when there is an a-z char, and then it returns to the start of the string :P).
                        If CHECK.Count > 0 Then
                            ' This means that we have, say, a y variable. If we are differentiating y^2, then eventually we will reach y as a product of the chain rule. The derivative of y is dy, and as I am automatically doing the ration over dx, it will become dy/dx. 
                            ' x has special treatment as it will become 1, thus I ignore it. This is the basic premise of multivariable calculus.
                            Dim TOP, BOTTOM As New TREE_NODE
                            Dim CLONED_SELECTED_NODE As TREE_NODE = SELECTED_NODE.CLONE
                            TOP.VALUE = "d" & SELECTED_NODE.VALUE
                            BOTTOM.VALUE = "dx"
                            CLONED_SELECTED_NODE.VALUE = "/"
                            CLONED_SELECTED_NODE.LEFT.Add(TOP)
                            CLONED_SELECTED_NODE.RIGHT.Add(BOTTOM)
                            Return CLONED_SELECTED_NODE
                        End If
                    End If
                Next
            Next
            Return PLUS_NODE

        Else
            Dim ZERO_NODE As New TREE_NODE
            ZERO_NODE.VALUE = "0"
            Return ZERO_NODE

        End If
        Return NODE
    End Function

    Private Sub DIFFERENTATION_WRAPPER(NODE As TREE_NODE)

        ' Where u^(A), and u is a variable of some kind (differentiable, ie if u = (2x^2+3) then implement chain rule) and A is a numeric power.
        ' Numeric powers are a prerequisite due to logarithms being required for any other form.
        ' This will be implemented elsewhere and added on in another prototype. 

        ' The form is of a "^", and the left node either being a * or a + node, and the right node being a number.
        ' d(u^a) -> a*u^(a-1)*du (normally derivatives are in the form d(f(x))/dx, but it is fine to revoke the ratio definer in this case for generalisation. In a more formal definition, df(u)=(d(f(u))/du) * du (typically u = some x, so you write du = (du/dx)*dx))
        ' This form is constant, so I will differentiate the +, or * node in any fashion.

        ' Where this must not work:
        ' Differentiation of this form MUST only occur on the foremost + node. 
        ' This will not be recursive. Ie, I will not look for nodes to differentiate beyond the root node.
        ' Ie, I can only differentiate "u+a", where u and a are of a common variable x (multivariable form is not an aim right now). I cannot differentiate "u+a" in "u^(u+a)", where I differentiate the bracket of the power, as this is wrong..


        If NODE.VALUE = "+" Then

            Dim COLLECTIVE = {NODE.LEFT, NODE.RIGHT}

            For Each ELEMENT In COLLECTIVE
                Dim CLONED_LOOP As New List(Of TREE_NODE)
                CLONED_LOOP.AddRange(ELEMENT)
                'Console.WriteLine("THE COUNTTTT" & CLONED_LOOP.Count)
                For A = 0 To ELEMENT.Count - 1

                    Dim SELECTED_NODE As TREE_NODE = CLONED_LOOP(A)
                    'Console.WriteLine("THIS THINGGG" & IN_ORDER(CLONED_LOOP(A), True))
                    Dim DIFFERENTIABLE = LIMITED_DIFFERENTIATE(SELECTED_NODE)
                    'Console.WriteLine("THIS THINGGzzzzG" & IN_ORDER(DIFFERENTIABLE, True))
                    ELEMENT.RemoveAt(A)
                    If DIFFERENTIABLE IsNot Nothing Then
                        ELEMENT.Insert(A, DIFFERENTIABLE)
                    End If
                Next
            Next
        Else
            Dim DIFFERENTIABLE As TREE_NODE = LIMITED_DIFFERENTIATE(NODE)
            Console.WriteLine("lol" & IN_ORDER(DIFFERENTIABLE, True) & NODE.VALUE)
            NODE.LEFT.RemoveRange(0, NODE.LEFT.Count)
            NODE.RIGHT.RemoveRange(0, NODE.RIGHT.Count)
            NODE.VALUE = DIFFERENTIABLE.VALUE
            NODE.LEFT.AddRange(DIFFERENTIABLE.LEFT)
            NODE.RIGHT.AddRange(DIFFERENTIABLE.RIGHT)
        End If

    End Sub


    Private Sub MULTIPLIER_SIMPLIFIER_CALCULATOR(TYPE As MULTIPLIER_NUM, ROOT_NODE As TREE_NODE, LEFT_MULTIPLIER_NODE As TREE_NODE, RIGHT_NODE As List(Of TREE_NODE))
        ' Two Cases

        ' This is setup in the way such that one single element node is sent (like 5x, in the form of a * or / node) and the rest of the RIGHT_NODE is sent.
        ' The RIGHT_NODE is a list. 

        ' In the case it isn't a + node, then it will just be, say, 5x * (a*b*c*...)
        ' Remember that it is assumed that the LEFT_MULTIPLIER_NODE comes from a + node from the left.
        ' In this instance it is very easy, and I will just move the resulting node (5*x*a*b*c*...) out of the + node and into the main LEFT NODE of the ROOT_NODE.
        ' This is because I assume the ROOT_NODE will turn into a + node by the end of it.

        ' In the case the RIGHT_NODE is a + NODE (ie it has 1 child and its a + NODE)
        ' I will loop through the RIGHT_NODE(0) (the + node) and for each item I will form a * node of them combined.
        ' I will then add this new node to the LEFT NODE/RIGHT NODE
        ' This continues until all nodes are done.

        Select Case TYPE
            Case MULTIPLIER_NUM.SUM ' This means its A*(B+C+D...)
                Dim ITERATION_ARRAY = {RIGHT_NODE(0).LEFT, RIGHT_NODE(0).RIGHT}
                For Each ELEMENT As List(Of TREE_NODE) In ITERATION_ARRAY
                    For Each NODE As TREE_NODE In ELEMENT
                        Dim NEW_NODE As New TREE_NODE
                        NEW_NODE.VALUE = "*"
                        NEW_NODE.LEFT.Add(LEFT_MULTIPLIER_NODE)
                        NEW_NODE.RIGHT.Add(NODE)
                        ROOT_NODE.LEFT.Add(NEW_NODE) ' Adds the new node into the LEFT child of the root.
                    Next
                Next
            Case MULTIPLIER_NUM.OTHER ' It is A*B
                Dim NEW_NODE As New TREE_NODE
                NEW_NODE.VALUE = "*"
                NEW_NODE.LEFT.Add(LEFT_MULTIPLIER_NODE)
                NEW_NODE.RIGHT.AddRange(RIGHT_NODE)
                ROOT_NODE.LEFT.Add(NEW_NODE)
        End Select

    End Sub

    'Private Sub 

    Private Sub COLLECT_LIKE_TERMS_WRAPPER(NODE As TREE_NODE)
        LEVEL_OPERATORS(NODE)
        PREPARATION_BY_TIMES_RULING(TREE_TO_MODIFY)
        COLLECT_LIKE_TERMS(NODE)
    End Sub


    Private Sub MULTIPLIER_WRAPPER_SOLVER(NODE As TREE_NODE)
        LEVEL_OPERATORS(NODE)
        MULTIPLIER_SOLVER(NODE)
    End Sub

    ' The 'Give Up' Subroutines
    ' Madness congealed
    ' Not made for elegance or efficiency TM.


    Private Sub REMOVE_POWERS(NODE As TREE_NODE)
        If Not NODE.LEFT Is Nothing Then
            For Each NODE_ELEMENT As TREE_NODE In NODE.LEFT
                REMOVE_POWERS(NODE_ELEMENT)
            Next
        End If
        If Not NODE.RIGHT Is Nothing Then
            For Each NODE_ELEMENT As TREE_NODE In NODE.RIGHT
                REMOVE_POWERS(NODE_ELEMENT)
            Next
        End If


        If NODE.VALUE = "^" Then
            If NODE.RIGHT(0).VALUE = "1" Then
                NODE.VALUE = NODE.LEFT(0).VALUE
                If Not IsNumeric(NODE.VALUE) Then
                    Dim TOTAL_NODE As New List(Of TREE_NODE)
                    TOTAL_NODE.AddRange(NODE.LEFT(0).LEFT)
                    TOTAL_NODE.AddRange(NODE.LEFT(0).RIGHT)
                    NODE.LEFT.RemoveRange(0, NODE.LEFT.Count)
                    NODE.RIGHT.RemoveRange(0, NODE.RIGHT.Count)
                    ALTERNATING_TREE_INSERTING(TOTAL_NODE, NODE)
                End If
            ElseIf NODE.RIGHT(0).VALUE = "0" Then
                NODE.VALUE = "1"
                NODE.LEFT.RemoveRange(0, NODE.LEFT.Count)
                NODE.RIGHT.RemoveRange(0, NODE.RIGHT.Count)
            End If
        End If

    End Sub

    Private Sub REMOVE_ZERO_MULTIPLIERS(NODE As TREE_NODE)
        If Not NODE.LEFT Is Nothing Then
            For Each NODE_ELEMENT As TREE_NODE In NODE.LEFT
                REMOVE_LOOSE_ADDITIONS(NODE_ELEMENT)
            Next
        End If
        If Not NODE.RIGHT Is Nothing Then
            For Each NODE_ELEMENT As TREE_NODE In NODE.RIGHT
                REMOVE_LOOSE_ADDITIONS(NODE_ELEMENT)
            Next
        End If
        Dim ITERABLE = {NODE.LEFT, NODE.RIGHT}


        If NODE.VALUE = "+" Then
            For Each ELEMENT As List(Of TREE_NODE) In ITERABLE
                For A = 0 To ELEMENT.Count - 1
                    If ELEMENT(A).VALUE = "0" Then

                    End If
                Next
            Next
        End If
    End Sub

    Private Sub REMOVE_LOOSE_ADDITIONS(NODE As TREE_NODE)
        If Not NODE.LEFT Is Nothing Then
            For Each NODE_ELEMENT As TREE_NODE In NODE.LEFT
                REMOVE_LOOSE_ADDITIONS(NODE_ELEMENT)
            Next
        End If
        If Not NODE.RIGHT Is Nothing Then
            For Each NODE_ELEMENT As TREE_NODE In NODE.RIGHT
                REMOVE_LOOSE_ADDITIONS(NODE_ELEMENT)
            Next
        End If

        If NODE.VALUE = "+" Then
            If (NODE.LEFT.Count + NODE.RIGHT.Count) <= 1 Then
                Dim CHOSEN_NODE As TREE_NODE
                If NODE.LEFT.Count = 1 Then
                    CHOSEN_NODE = NODE.LEFT(0)
                ElseIf NODE.RIGHT.Count = 1 Then
                    CHOSEN_NODE = NODE.RIGHT(0)
                Else
                    Return
                End If
                NODE.VALUE = CHOSEN_NODE.VALUE
                If IsNumeric(CHOSEN_NODE.VALUE) Then
                    NODE.LEFT.RemoveRange(0, NODE.LEFT.Count)
                    NODE.RIGHT.RemoveRange(0, NODE.RIGHT.Count)
                Else
                    Dim NEW_READDING_LIST As New List(Of TREE_NODE)
                    NEW_READDING_LIST.AddRange(CHOSEN_NODE.LEFT)
                    NEW_READDING_LIST.AddRange(CHOSEN_NODE.RIGHT)
                    NODE.LEFT.RemoveRange(0, NODE.LEFT.Count)
                    NODE.RIGHT.RemoveRange(0, NODE.RIGHT.Count)
                    ALTERNATING_TREE_INSERTING(NEW_READDING_LIST, NODE)
                End If
            End If
        End If
    End Sub

    Private Sub CLEANUP_FINALISER(NODE As TREE_NODE)

        ' Let A*1 = A

        If Not NODE.LEFT Is Nothing Then
            For Each NODE_ELEMENT As TREE_NODE In NODE.LEFT
                CLEANUP_FINALISER(NODE_ELEMENT)
            Next
        End If
        If Not NODE.RIGHT Is Nothing Then
            For Each NODE_ELEMENT As TREE_NODE In NODE.RIGHT
                CLEANUP_FINALISER(NODE_ELEMENT)
            Next
        End If


        If NODE.VALUE = "+" Then
            Dim INTERATION_LIST = {NODE.LEFT, NODE.RIGHT}
            For Each ELEMENT_LIST As List(Of TREE_NODE) In INTERATION_LIST
                Dim BUFFER As Integer = 0
                For A = 0 To ELEMENT_LIST.Count - 1
                    If (A - BUFFER) <= ELEMENT_LIST.Count - 1 Then
                        Dim TRUE_POINTER As Integer = A - BUFFER
                        If ELEMENT_LIST(TRUE_POINTER).VALUE = "*" Then ' If its a * node, so something like 0*x
                            Dim TOTAL_LIST As New List(Of TREE_NODE)
                            TOTAL_LIST.AddRange(ELEMENT_LIST(TRUE_POINTER).RIGHT)
                            TOTAL_LIST.AddRange(ELEMENT_LIST(TRUE_POINTER).LEFT)
                            Dim ZEROS = From INT In TOTAL_LIST Where INT.VALUE = "0" ' Collects all the 0's in the node, if there are any.
                            If ZEROS.Count > 0 Then
                                ELEMENT_LIST.RemoveAt(TRUE_POINTER)
                                If NODE.LEFT.Count + NODE.RIGHT.Count = 0 Then
                                    NODE.VALUE = ""
                                End If
                                BUFFER += 1
                            End If
                        ElseIf ELEMENT_LIST(TRUE_POINTER).VALUE = "0" Then ' Just a 0
                            ELEMENT_LIST.RemoveAt(TRUE_POINTER)
                            BUFFER += 1
                        End If
                    End If
                Next
            Next
        End If

        If NODE.VALUE = "*" Then
            If NODE.RIGHT.Count = 1 Then
                ' Console.WriteLine("FUCKJING HELL WHY ARE YOU HERRE" & ELEMENT(A).RIGHT(0).VALUE)
                If NODE.RIGHT(0).VALUE = "1" Then
                    NODE.RIGHT.RemoveAt(0)
                    If Not NODE.LEFT.Count = 1 Then
                        Dim NEW_READDING_LIST As New List(Of TREE_NODE)
                        NEW_READDING_LIST.AddRange(NODE.LEFT)
                        NODE.LEFT.RemoveRange(0, NODE.LEFT.Count)
                        ALTERNATING_TREE_INSERTING(NEW_READDING_LIST, NODE) 'Adds it via alternate insertion (even number of items on left and right) so it displays correctly.
                    ElseIf NODE.LEFT.Count = 1 And IsNumeric(NODE.LEFT(0).VALUE) Then
                        NODE.VALUE = NODE.LEFT(0).VALUE
                        NODE.LEFT.RemoveAt(0)
                    Else
                        NODE.VALUE = NODE.LEFT(0).VALUE
                        Dim NEW_READDING_LIST As New List(Of TREE_NODE)
                        NEW_READDING_LIST.AddRange(NODE.LEFT(0).LEFT)
                        NEW_READDING_LIST.AddRange(NODE.LEFT(0).RIGHT)
                        NODE.LEFT.RemoveRange(0, NODE.LEFT.Count)
                        ALTERNATING_TREE_INSERTING(NEW_READDING_LIST, NODE)
                    End If
                End If
            End If
        End If

    End Sub

    Private Sub POWER_SOLVER(NODE As TREE_NODE)
        ' Let x^n*x^b = x^(n+b)

        If Not NODE.LEFT Is Nothing Then
            For Each NODE_ELEMENT As TREE_NODE In NODE.LEFT
                POWER_SOLVER(NODE_ELEMENT)
            Next
        End If
        If Not NODE.RIGHT Is Nothing Then
            For Each NODE_ELEMENT As TREE_NODE In NODE.RIGHT
                POWER_SOLVER(NODE_ELEMENT)
            Next
        End If

        Dim NODE_LIST As New List(Of TREE_NODE)

        If NODE.VALUE = "*" Then
            Dim ITERABLE = {NODE.LEFT, NODE.RIGHT}
            For Each ELEMENT As List(Of TREE_NODE) In ITERABLE
                Dim DOWN_BREAK As Integer = 0
                For A As Integer = 0 To ELEMENT.Count - 1
                    Dim LESSENED_INDEX As Integer = A - DOWN_BREAK
                    If (LESSENED_INDEX) <= ELEMENT.Count - 1 Then ' Makes sure that it is within bounds ;)
                        If ELEMENT(LESSENED_INDEX).VALUE = "^" Then
                            NODE_LIST.Add(ELEMENT(LESSENED_INDEX))
                            ELEMENT.RemoveAt(LESSENED_INDEX)
                            DOWN_BREAK += 1 ' Shifts the loop the left by 1, so that we ain't losing any looping buddies :0
                        End If
                    End If
                Next
            Next
            ' "^" nodes are binary, so we can only have 0 indexes for left and right.
            Dim GROUPED = NODE_LIST.GroupBy(Function(x) IN_ORDER(x.LEFT(0), True)) ' Off did the computer go, for this program was trying to turn it into a toaster. (It groups each by the same variables, ie {{x},{x^3,x^2,x^1}})
            For Each GROUP In GROUPED ' We gonna loop through it all baby
                Dim MAIN_VARIABLE As TREE_NODE = GROUP.ElementAt(0).LEFT(0)
                Dim MAIN_POWER As New TREE_NODE
                MAIN_POWER.VALUE = "+" ' This is weird but I will just solve for this + node like I do for an input :)

                Dim ALTERNATING_EVEN_NUMBER As Integer = 1
                Dim CHECK As New Dictionary(Of String, String)
                For Each ITEM As TREE_NODE In GROUP
                    CHECK = MATCH_COLLECTION_TO_DICTIONARY(Regex.Matches(ITEM.VALUE, "[*,+,/,-,^]"))
                    If CHECK.Count = 0 Then
                        MAIN_POWER.LEFT.Add(ITEM.RIGHT(0))
                    Else
                        MAIN_POWER.RIGHT.Add(ITEM.RIGHT(0))
                    End If
                    ALTERNATING_EVEN_NUMBER += 1
                Next
                'LEVEL_OPERATORS(MAIN_POWER)

                ' Dim SIMPLIFIED_TREE_NODE As TREE_NODE = OPTIMISE_TREE(MAIN_POWER.CLONE) ' Optimise the tree. Technically recursively. Gosh.
                ' Console.WriteLine("IDKFv2" & IN_ORDER(SIMPLIFIED_TREE_NODE, True))
                Dim FINAL_NODE As New TREE_NODE
                FINAL_NODE.VALUE = "^"
                FINAL_NODE.LEFT.Insert(0, MAIN_VARIABLE)
                FINAL_NODE.RIGHT.Insert(0, MAIN_POWER)
                CHECK = MATCH_COLLECTION_TO_DICTIONARY(Regex.Matches(MAIN_VARIABLE.VALUE, "[*,+,/,-,^]"))
                If CHECK.Count = 0 Then
                    NODE.LEFT.Add(FINAL_NODE)
                Else
                    NODE.RIGHT.Add(FINAL_NODE)
                End If
            Next




        End If
    End Sub
    Private Sub MULTIPLIER_SOLVER(NODE As TREE_NODE)
        ' Let A*B = C, when A and B are integers.

        If Not NODE.LEFT Is Nothing Then
            For Each NODE_ELEMENT As TREE_NODE In NODE.LEFT
                MULTIPLIER_SOLVER(NODE_ELEMENT)
            Next
        End If
        If Not NODE.RIGHT Is Nothing Then
            For Each NODE_ELEMENT As TREE_NODE In NODE.RIGHT
                MULTIPLIER_SOLVER(NODE_ELEMENT)
            Next
        End If
        If NODE.VALUE = "*" Then
            Dim TOTAL_LIST As New List(Of TREE_NODE)
            TOTAL_LIST.AddRange(NODE.RIGHT)
            TOTAL_LIST.AddRange(NODE.LEFT)
            Dim INTEGERS = From INT In TOTAL_LIST Where IsNumeric(INT.VALUE)
            Dim FINAL_INT As Integer = 1
            For A = 0 To INTEGERS.Count - 1
                FINAL_INT *= INTEGERS(0).VALUE
                TOTAL_LIST.Remove(INTEGERS(0))
            Next
            Dim NEW_INT As New TREE_NODE
            If FINAL_INT <> "1" Or TOTAL_LIST.Count = 0 Then
                NEW_INT.VALUE = FINAL_INT
                TOTAL_LIST.Insert(0, NEW_INT)
            End If
            NODE.LEFT = New List(Of TREE_NODE)
                NODE.RIGHT = New List(Of TREE_NODE)
                Dim CHECK As Dictionary(Of String, String) = MATCH_COLLECTION_TO_DICTIONARY(Regex.Matches(TOTAL_LIST(0).VALUE, "[*,+,/,-,^]"))
                If TOTAL_LIST.Count = 1 And ((IsNumeric(TOTAL_LIST(0).VALUE) Or CHECK.Count = 0)) Then
                    NODE.VALUE = TOTAL_LIST(0).VALUE
                ElseIf TOTAL_LIST.Count >= 1 Then
                    For Each ITEM_IN_LIST As TREE_NODE In TOTAL_LIST
                        CHECK = MATCH_COLLECTION_TO_DICTIONARY(Regex.Matches(ITEM_IN_LIST.VALUE, "[*,+,/,-,^]"))
                        If CHECK.Count = 0 Then ' Its a number or variable.
                            NODE.LEFT.Add(ITEM_IN_LIST)
                        Else
                            NODE.RIGHT.Add(ITEM_IN_LIST)
                        End If
                    Next
                End If
            End If
    End Sub

    ' End of the 'Give Up' Subroutines

    Private Function MULTIPLIER_SIMPLIFIER(NODE As TREE_NODE)


        Dim NODE_LIST As New List(Of TREE_NODE) 'Used so that I can easily compare.
        Dim MODE As MULTIPLIER_NUM

        ' The objective of this is to remove bracket multiplication.
        ' Ie, when either a bracket is multiplied by a single value, or there are 2 brackets.
        ' When such a scenarior exists, it will loop through the discovered bracket.

        ' On this instance there are two occurences - 

        ' <A> the other node is a * or a / node. This means I can merely create a multiplication node for each element being multiplied.
        ' Ie A*(B+C) = A*B + B*C. I will transform the multiplication node into a + node to house the subsequent calculations.

        ' <B> the other node is a + node.
        ' This means I must loop through each individual node within the other.
        ' Ie (A+B)(C+D) = (A*C + A*D) + (B*C + B*D). 


        If Not NODE.LEFT Is Nothing Then
            For Each NODE_ELEMENT As TREE_NODE In NODE.LEFT
                MULTIPLIER_SIMPLIFIER(NODE_ELEMENT)
            Next
        End If
        If Not NODE.RIGHT Is Nothing Then
            For Each NODE_ELEMENT As TREE_NODE In NODE.RIGHT
                MULTIPLIER_SIMPLIFIER(NODE_ELEMENT)
            Next
        End If


        If NODE.VALUE = "*" Then
            ' Console.WriteLine("DETECTED THE SHIT" & IN_ORDER(NODE, True) & NODE.LEFT.Count & NODE.RIGHT.Count & NODE.RIGHT(0).VALUE)

            Dim ITERATION_LIST = {NODE.LEFT, NODE.RIGHT} ' This is a slight modification to allow >2 brackets in a multiplier.
            ' If I have, say, (9x+3)*(10x-3)*8x, then I will first select (9x+3) and compare it to (10x-3) and 8x, both are doable, but the algorithm will first evaluate (9x+3)*(10x-3).
            ' It will the return the result as one bracket.

            For Each ELEMENT_LIST As List(Of TREE_NODE) In ITERATION_LIST

                Dim BUFFER As Integer = 0
                For A = 0 To ELEMENT_LIST.Count - 1
                    If ELEMENT_LIST.Count = 0 Then
                        Exit For
                    End If
                    ' As whenever there is a successful operation, the count is reduced by 1, I must add a buffer to properly reflect it.
                    Dim SELECTED_NODE As TREE_NODE = ELEMENT_LIST(A - BUFFER)

                    ' Now I must compare this selected node with every other node in the list.
                    Dim EVERY_OTHER_NODE As New List(Of TREE_NODE)
                    EVERY_OTHER_NODE.AddRange(NODE.LEFT)
                    EVERY_OTHER_NODE.AddRange(NODE.RIGHT)
                    EVERY_OTHER_NODE.Remove(SELECTED_NODE) '  Removes the one I dont want.


                    For C = 0 To EVERY_OTHER_NODE.Count - 1

                        Dim TEMPORARY_NODE As New TREE_NODE
                        TEMPORARY_NODE.LEFT.Add(SELECTED_NODE)
                        TEMPORARY_NODE.RIGHT.Add(EVERY_OTHER_NODE(C))
                        TEMPORARY_NODE.VALUE = "*"
                        ' I will simulate a binary multiplier so that it will accomodate for previous code I wrote (that only works with 'two' multipliers, like 9x*(10x+3) or something).

                        If TEMPORARY_NODE.RIGHT.Count = 1 Then
                            If TEMPORARY_NODE.RIGHT(0).VALUE = "+" Then
                                MODE = MULTIPLIER_NUM.SUM
                            Else
                                MODE = MULTIPLIER_NUM.OTHER
                            End If
                        Else
                            MODE = MULTIPLIER_NUM.OTHER
                        End If

                        If TEMPORARY_NODE.LEFT.Count = 1 Then
                            If TEMPORARY_NODE.VALUE = "+" Then ' These two checks mean I have a (a+b+c+...) form for my left node

                                Dim ITERATION_ARRAY = {TEMPORARY_NODE.LEFT(0).LEFT, TEMPORARY_NODE.LEFT(0).RIGHT}
                                For Each ELEMENT As List(Of TREE_NODE) In ITERATION_ARRAY ' Sum Nodes have left and right.
                                    For B As Integer = 0 To ELEMENT.Count() - 1
                                        If Not ELEMENT(B) Is Nothing Then ' Makes sure it exists.
                                            Dim NODE_ELEMENT As TREE_NODE = ELEMENT(B)

                                            ' I have the node I want to multiply.
                                            MULTIPLIER_SIMPLIFIER_CALCULATOR(MODE, TEMPORARY_NODE, NODE_ELEMENT, TEMPORARY_NODE.RIGHT)
                                            BUFFER += 1
                                        End If
                                    Next
                                Next
                                TEMPORARY_NODE.VALUE = "+"
                                TEMPORARY_NODE.LEFT.RemoveAt(0)
                                TEMPORARY_NODE.RIGHT = New List(Of TREE_NODE)
                            End If
                        End If
                        If TEMPORARY_NODE.LEFT.Count >= 1 And TEMPORARY_NODE.RIGHT.Count = 1 Then ' This is the 1 scenario the top if statement doesn't take into account.
                            ' I have had some preperations done beforehand so that all the * nodes have the + nodes on the right.
                            Dim BEFORE_COUNT As Integer = TEMPORARY_NODE.LEFT.Count
                            '  Console.WriteLine("DETECTED THE SHIT" & IN_ORDER(NODE, True))
                            If TEMPORARY_NODE.RIGHT(0).VALUE = "+" Then ' These two checks mean I have a (a+b+c+...) form for my right node, ie A*(x+y+z)
                                ' Console.WriteLine("DETECTED THE SHIT")
                                Dim NEW_NODE As New TREE_NODE
                                NEW_NODE.VALUE = "*"
                                If TEMPORARY_NODE.LEFT.Count = 2 Then
                                    NEW_NODE.LEFT.Add(TEMPORARY_NODE.LEFT(0))
                                    NEW_NODE.RIGHT.Add(TEMPORARY_NODE.LEFT(1))
                                ElseIf TEMPORARY_NODE.LEFT.Count > 2 Then
                                    NEW_NODE.LEFT = TEMPORARY_NODE.LEFT.GetRange(0, TEMPORARY_NODE.LEFT.Count - 1) ' Splits the newnode so it is displayed properly.
                                    NEW_NODE.RIGHT = TEMPORARY_NODE.LEFT.GetRange(TEMPORARY_NODE.LEFT.Count - 1, 1)
                                Else
                                    NEW_NODE.LEFT.Add(TEMPORARY_NODE.LEFT(0))
                                    Dim ONE_NODE As New TREE_NODE
                                    ONE_NODE.VALUE = "1"
                                    NEW_NODE.RIGHT.Add(ONE_NODE)
                                End If



                                MULTIPLIER_SIMPLIFIER_CALCULATOR(MODE, TEMPORARY_NODE, NEW_NODE.CLONE(), TEMPORARY_NODE.RIGHT)
                                TEMPORARY_NODE.VALUE = "+"
                                TEMPORARY_NODE.LEFT.RemoveRange(0, BEFORE_COUNT)
                                TEMPORARY_NODE.RIGHT = New List(Of TREE_NODE)
                                TEMPORARY_NODE.RIGHT.Add(TEMPORARY_NODE.LEFT(0))
                                TEMPORARY_NODE.LEFT.RemoveAt(0)
                            End If
                        End If

                        If TEMPORARY_NODE.VALUE = "+" Then ' This means that there was a successful calculation.
                            Console.WriteLine("done lol")
                            ' We will remove the two nodes that were previously in the temporary node
                            ELEMENT_LIST.Remove(SELECTED_NODE)
                            NODE.LEFT.Remove(EVERY_OTHER_NODE(C))
                            NODE.RIGHT.Remove(EVERY_OTHER_NODE(C))
                            If NODE.LEFT.Count + NODE.RIGHT.Count = 0 Then ' I will replace this node with the formed node, as there will only be 1 element in the * node.
                                NODE.VALUE = TEMPORARY_NODE.VALUE
                                NODE.LEFT.AddRange(TEMPORARY_NODE.LEFT)
                                NODE.RIGHT.AddRange(TEMPORARY_NODE.RIGHT)
                                TEMPORARY_NODE.LEFT.RemoveRange(0, TEMPORARY_NODE.LEFT.Count)
                                TEMPORARY_NODE.RIGHT.RemoveRange(0, TEMPORARY_NODE.RIGHT.Count)
                                TEMPORARY_NODE = Nothing
                            Else
                                NODE.RIGHT.Add(TEMPORARY_NODE)
                            End If
                            BUFFER += 1 ' Set the buffer.
                            Exit For 'Exit the for 
                        End If


                    Next
                Next

            Next
        End If


        Return NODE

    End Function

    Private Sub FRACTION_COLLECTER_WRAPPER(NODE As TREE_NODE)
        ' As the function 'SIMPLE_COLLECT_FRACTION_DENOMINATORS' does not level operators when it creates Addition Nodes.
        LEVEL_OPERATORS(NODE)
        SIMPLE_COLLECT_FRACTION_DENOMINATORS(NODE)
    End Sub

    Private Sub CLEANUP_ADDITION(NODE As TREE_NODE)

    End Sub

    Private Function SIMPLE_COLLECT_FRACTION_DENOMINATORS(NODE As TREE_NODE)
        ' This is made to combine fractions with the same denominator.
        ' It should be a fairly simple scenarior to solve, and the reason for its existence is for the like term collector to work properly.
        ' There are a lot more nuances to this solution, though, so I named this one 'Simple'.
        ' A 'common denominator' can normally be found with ease.
        ' a/b + c/d, I need to multiply by every other denominator for each term. a/b * d/d. c/d * b/b. 

        ' This function was made before any Multiplication Solver was implemented, so it is crippled in functionality.
        ' I do not intend to make a fully featured simplifier, so this is fine.
        ' ~I'll leave it at that before I enter a self serving tangent and realise the enormity of what I'm attempting to 'partially' solve.

        ' I decided to use a slightly more efficient method compared to COLLECT_LIKE_TERMS.
        ' It is a pre-order traversal.

        Dim ITERATION_ARRAY = {NODE.LEFT, NODE.RIGHT}
        Dim NODE_LIST As New List(Of TREE_NODE) 'Used so that I can easily compare.



        For Each ELEMENT As List(Of TREE_NODE) In ITERATION_ARRAY ' This will just loop through all the nodes and recursively call them.
            ' To be fair I can probably change all the functions below to this form, but it will become a little annoying to read.
            If NODE.VALUE = "+" Then
                ' Imagine a circumstance where there is a + node and a bunch of variables. Even if I am performing a recursive call, eventually they will all terminate to the nodes in the + node.
                ' Thus I will be able to list all the nodes that need to be compared, all while having it do it to all plus nodes, as it's recursive. 
                ' Wunderbar!
                For A As Integer = 0 To ELEMENT.Count() - 1
                    If (A) <= (ELEMENT.Count - 1) Then
                        Dim TREE_ELEMENT As TREE_NODE = COLLECT_LIKE_TERMS(ELEMENT(A))
                        If TREE_ELEMENT.VALUE = "/" Then
                            For B As Integer = 0 To NODE_LIST.Count() - 1 ' Same method as COLLECT_LIKE_TERMS
                                If IN_ORDER(TREE_ELEMENT.RIGHT(0), True) = IN_ORDER(NODE_LIST(B).RIGHT(0), True) Then ' Comparing denominators.
                                    Dim NEW_NODE, ADD_NODE As New TREE_NODE
                                    ADD_NODE.VALUE = "+"

                                    NEW_NODE.VALUE = "/"
                                    NEW_NODE.RIGHT.Add(TREE_ELEMENT.RIGHT(0)) ' I add the common denominator
                                    NEW_NODE.LEFT.Add(ADD_NODE) ' Division is binary, so I add a sum node for the two numerators being added.

                                    NEW_NODE.LEFT(0).LEFT.Add(TREE_ELEMENT.LEFT(0)) ' I add the numerators to this sum node, in left and right.
                                    NEW_NODE.LEFT(0).RIGHT.Add(NODE_LIST(B).LEFT(0))
                                    ELEMENT.Remove(TREE_ELEMENT)
                                    ITERATION_ARRAY(1).Remove(NODE_LIST(B)) ' Removes the node_list node from the array. Badly.
                                    ITERATION_ARRAY(0).Remove(NODE_LIST(B))

                                    NODE_LIST.RemoveAt(B)
                                    NODE_LIST.Add(NEW_NODE) ' Adds the new node to be compared in the node list.

                                    ELEMENT.Add(NEW_NODE) ' Adds the new node.
                                    Exit For
                                Else
                                    NODE_LIST.Add(TREE_ELEMENT)
                                End If
                            Next
                            If NODE_LIST.Count = 0 Then
                                NODE_LIST.Add(TREE_ELEMENT)
                            End If
                        End If
                    End If
                Next
            Else
                For A As Integer = 0 To ELEMENT.Count() - 1
                    COLLECT_LIKE_TERMS(ELEMENT(A)) 'Go down and down and down
                Next
            End If
        Next

        Return NODE
    End Function

    Structure SEPERATED_TERMS
        Public COEFFICIENT As Integer
        Public VARIABLE As TREE_NODE
    End Structure

    Private Function COLLECT_LIKE_TERMS(NODE As TREE_NODE)
        ' To collect like terms there are certain rules.
        ' For example, 3xy + 2xy^2 cannot be collected as they do not have the same variable.
        ' For trees I must identify the variables of each 'term' and be able to compare whether they are able to collected.
        ' This of course can only happen on + modifiers, and due to my simplifying beforehand I do not need to worry about '-' nodes.

        ' To identify terms - 
        ' I already know that a single term will can be represented as 3 f g (...) *, where f and g are some functions. 
        ' Thus, every term will eventually terminate to a multplication node, or to a number if it is not a variable.
        ' Therefore, when I eventually encounter an execution Stack Item where my root is a +, and my children are either numbers or variables, I will be able to compare them.

        ' I will use a pre order search for this circumstance.

        Dim ITERATION_ARRAY = {NODE.LEFT, NODE.RIGHT}
        Dim NODE_LIST As New List(Of TREE_NODE) 'Used so that I can easily compare.
        Dim ADDED = False
        ' I consider this an 'Up to Down' Method.


        If NODE.VALUE = "+" Then ' Like terms can be collected.
            For Each ELEMENT As List(Of TREE_NODE) In ITERATION_ARRAY
                For A As Integer = 0 To ELEMENT.Count() - 1
                    If (A) <= (ELEMENT.Count - 1) Then
                        Dim ELEMENT_A As TREE_NODE = ELEMENT(A) ' The element I will compare against all the others within the NODE_LIST.
                        If ELEMENT_A.VALUE <> "+" Then
                            Dim ELEMENT_TOTAL, ELEMENT_B As TREE_NODE
                            ' Console.WriteLine(IN_ORDER(ELEMENT_A, True) & " juh ")
                            For Each ELEMENT_B_TEMP As TREE_NODE In NODE_LIST ' I will now compare it to every node I HAVE looked at. (This is better than, say, comparing it to everything from the getgo.)
                                Dim TERMS_A, TERMS_B As SEPERATED_TERMS
                                TERMS_A = RETURN_VARIABLE_AND_COEFFICIENT(ELEMENT_A.CLONE())
                                TERMS_B = RETURN_VARIABLE_AND_COEFFICIENT(ELEMENT_B_TEMP.CLONE())
                                'Console.WriteLine(IN_ORDER(ELEMENT_A, True) & " WHAT " & IN_ORDER(ELEMENT_B_TEMP, True))


                                Dim ALL_TERMS_A As New List(Of TREE_NODE)
                                ALL_TERMS_A.AddRange(TERMS_A.VARIABLE.LEFT)
                                ALL_TERMS_A.AddRange(TERMS_A.VARIABLE.RIGHT)
                                Dim ALL_TERMS_B As New List(Of TREE_NODE)
                                ALL_TERMS_B.AddRange(TERMS_B.VARIABLE.LEFT)
                                ALL_TERMS_B.AddRange(TERMS_B.VARIABLE.RIGHT)

                                Dim STRING_TERMS_A = (From ITEM In ALL_TERMS_A Select IN_ORDER(ITEM, True)).ToList()

                                Dim STRING_TERMS_B = (From ITEM In ALL_TERMS_B Select IN_ORDER(ITEM, True)).ToList() ' This allows me to compare the two lists, so that in cases where I have 9xy + 9yx, I can still find they have the same variables.


                                If ALL_TERMS_A.Count = ALL_TERMS_B.Count Then ' They must have the same number of variables.
                                    Dim COUNT_SIMILAR As Integer = 0
                                    For Each ITEM In STRING_TERMS_A
                                        STRING_TERMS_B.Remove(ITEM)
                                    Next

                                    If STRING_TERMS_B.Count = 0 Then ' If I just compared their string then it wouldnt allow 9xy + 9yx as although they're the same, the string wont be.
                                        Console.WriteLine(IN_ORDER(TERMS_A.VARIABLE, True) & " " & IN_ORDER(TERMS_B.VARIABLE, True))
                                        Dim NEW_NODE As New TREE_NODE
                                        NEW_NODE.VALUE = TERMS_A.COEFFICIENT + TERMS_B.COEFFICIENT ' This will be the new coefficient
                                        ' Console.WriteLine("FINALISED THING" & IN_ORDER(TERMS_B.VARIABLE, True) & "NODE ROOT" & TERMS_B.VARIABLE.LEFT(0).VALUE)
                                        If TERMS_B.VARIABLE.VALUE = "*" Then
                                            TERMS_B.VARIABLE.LEFT.Insert(0, NEW_NODE) ' I will add the coefficient to the front of the node's variable. 
                                        ElseIf TERMS_B.VARIABLE.VALUE = "/" Then ' As "/" is binary, then I must insert it into the extra node beyond the left node. Say * or +. Ie, its some addition, or multiplication, divided by something else.
                                            TERMS_B.VARIABLE.LEFT(0).LEFT.Insert(0, NEW_NODE) ' I'll go left in the left because who likes it right.
                                        End If
                                        ELEMENT_TOTAL = TERMS_B.VARIABLE ' This will be the new node.
                                        ELEMENT_B = ELEMENT_B_TEMP ' This is here so I can remove it.
                                    End If
                                End If
                            Next

                            If Not ELEMENT_B Is Nothing Then ' As it is an 'each' loop, I will need to modify it outside of it. 
                                NODE_LIST.Remove(ELEMENT_B)
                                If ELEMENT.Contains(ELEMENT_B) Then
                                    ELEMENT.Remove(ELEMENT_B)
                                Else
                                    ITERATION_ARRAY(1).Remove(ELEMENT_B) ' "It just works." TM
                                    ITERATION_ARRAY(0).Remove(ELEMENT_B) ' If the list becomes massive then this will become a problem. It won't become massive.
                                End If
                                ELEMENT.Remove(ELEMENT_A)
                                If NODE.LEFT.Count > NODE.RIGHT.Count Then
                                    NODE.RIGHT.Add(ELEMENT_TOTAL)
                                Else
                                    NODE.LEFT.Add(ELEMENT_TOTAL)
                                End If
                            Else ' This means no like term could be found. Thus I will just add ELEMENT_A to the NODE_LIST to be compared to the rest.
                                NODE_LIST.Add(ELEMENT_A)
                            End If
                            ELEMENT_B = Nothing
                            ELEMENT_A = Nothing
                        End If
                    End If
                Next
            Next
        End If
        If Not NODE.LEFT Is Nothing Then
            For A As Integer = 0 To NODE.LEFT.Count() - 1
                COLLECT_LIKE_TERMS(NODE.LEFT(A)) 'A simple traversal, which is nice.
            Next
        End If

        If Not NODE.RIGHT Is Nothing Then
            For A As Integer = 0 To NODE.RIGHT.Count() - 1
                COLLECT_LIKE_TERMS(NODE.RIGHT(A)) ' This goes down each path.
            Next
        End If



        Return NODE


    End Function

    Private Function RETURN_VARIABLE_AND_COEFFICIENT(NODE As TREE_NODE)
        ' This will go through a node and remove the coefficient. It returns an array of the cleaned node and the coefficient.
        ' Do not use this on a + node, as that is not okay and hurts me in many ways.
        ' UPDATE -I can not be bothered to stop it from doing that, but it doesn't seem to do much except waste resources. Probably fine, but I won't test it to make sure. That would give me a reason to fix it.
        Dim COEFFICIENT As Integer = 1
        Dim RETURN_STRUCTURE As New SEPERATED_TERMS

        'Console.WriteLine("THIS IS WHAT WILL BE SOPRTED " & IN_ORDER(NODE, True))
        If Not NODE.VALUE = "+" Then
            If Not NODE.LEFT Is Nothing Then
                For A As Integer = 0 To NODE.LEFT.Count() - 1
                    If A <= (NODE.LEFT.Count - 1) And NODE.VALUE <> "^" Then ' Makes sure it exists.
                        Dim NODE_ELEMENT_S As SEPERATED_TERMS = RETURN_VARIABLE_AND_COEFFICIENT(NODE.LEFT(A))
                        Dim NODE_ELEMENT As TREE_NODE = NODE_ELEMENT_S.VARIABLE
                        COEFFICIENT *= NODE_ELEMENT_S.COEFFICIENT
                        If IsNumeric(NODE_ELEMENT.VALUE) Then
                            NODE.LEFT.RemoveAt(A) ' Removes the coefficient
                            COEFFICIENT *= NODE_ELEMENT.VALUE
                            ' Console.WriteLine("addd 1" & COEFFICIENT)
                        End If
                        If NODE_ELEMENT.RIGHT.Count = 1 And NODE_ELEMENT.LEFT.Count = 1 Then
                            If NODE_ELEMENT.VALUE = "*" And NODE_ELEMENT.LEFT(0).VALUE = "-1" Then ' This is a negative number
                                NODE.LEFT.RemoveAt(A) ' Removes the coefficient
                                COEFFICIENT *= -NODE_ELEMENT.RIGHT(0).VALUE
                                'Console.WriteLine("addd 2" & COEFFICIENT)
                            End If
                        End If
                    End If
                Next
            End If

            If Not NODE.RIGHT Is Nothing Then
                For A As Integer = 0 To NODE.RIGHT.Count() - 1
                    If NODE.RIGHT.Count = 1 And IsNumeric(NODE.RIGHT(0).VALUE) Then ' This takes into account that a number will be in the form (a*b, where a is the coefficient and b "1"
                        If NODE.RIGHT(0).VALUE = 1 Then
                            Exit For
                        End If
                    End If
                    If A <= (NODE.RIGHT.Count - 1) And NODE.VALUE <> "^" Then  ' Makes sure it exists.
                        Dim NODE_ELEMENT_S As SEPERATED_TERMS = RETURN_VARIABLE_AND_COEFFICIENT(NODE.RIGHT(A))
                        Dim NODE_ELEMENT As TREE_NODE = NODE_ELEMENT_S.VARIABLE
                        COEFFICIENT *= NODE_ELEMENT_S.COEFFICIENT
                        If IsNumeric(NODE_ELEMENT.VALUE) Then
                            NODE.RIGHT.RemoveAt(A) ' Removes the coefficient
                            COEFFICIENT *= NODE_ELEMENT.VALUE
                        End If
                        If NODE_ELEMENT.RIGHT.Count = 1 And NODE_ELEMENT.LEFT.Count = 1 Then
                            If NODE_ELEMENT.VALUE = "*" And NODE_ELEMENT.LEFT(0).VALUE = "-1" Then ' This is a negative number
                                NODE.RIGHT.RemoveAt(A) ' Removes the coefficient
                                COEFFICIENT *= -NODE_ELEMENT.RIGHT(0).VALUE
                            End If
                        End If
                    End If
                Next
            End If
        End If
        'Console.WriteLine("RETURNED COF " & COEFFICIENT)
        RETURN_STRUCTURE.COEFFICIENT = COEFFICIENT
        RETURN_STRUCTURE.VARIABLE = NODE
        Return RETURN_STRUCTURE

    End Function


    ' Trivial Flattening functions

    Private Sub SUB_RATIONAL_FUNCTION(PARENT_NODE As TREE_NODE, NODE As TREE_NODE, TYPE As ETYPE) ' TYPE is the orientation of the child, left or right.
        ' First Case
        ' Division nodes are binary!
        If TYPE = ETYPE.LEFT And NODE.VALUE = "/" And PARENT_NODE.VALUE = "/" Then ' The child is the numerator and both are division
            Dim NUMERATOR_CHILD As List(Of TREE_NODE) ' The left children. This will only be one element, but due to my system it is still in a list.
            NUMERATOR_CHILD = NODE.LEFT ' This is the numerator of the child. 
            Dim PRODUCT_TREE As New TREE_NODE
            PRODUCT_TREE.VALUE = "*"
            PRODUCT_TREE.LEFT = NODE.RIGHT
            PRODUCT_TREE.RIGHT = PARENT_NODE.RIGHT ' This is the product node. 
            PARENT_NODE.LEFT = NUMERATOR_CHILD
            Dim HOLDER As New List(Of TREE_NODE)
            HOLDER.Add(PRODUCT_TREE)
            PARENT_NODE.RIGHT = HOLDER ' Sets the same
            ' This equates to the same thing, but with only one division node.
        End If

        ' Second Case
        If TYPE = ETYPE.RIGHT And NODE.VALUE = "/" And PARENT_NODE.VALUE = "/" Then
            Dim PRODUCT_TREE As New TREE_NODE
            PRODUCT_TREE.VALUE = "*"
            PRODUCT_TREE.LEFT = PARENT_NODE.LEFT
            PRODUCT_TREE.RIGHT = NODE.RIGHT ' This is the product node. 
            Dim DENOMINATOR As List(Of TREE_NODE)
            DENOMINATOR = NODE.LEFT ' The denominator of the parent is the numerator of the child.
            Dim HOLDER As New List(Of TREE_NODE)
            HOLDER.Add(PRODUCT_TREE)
            PARENT_NODE.LEFT = HOLDER
            PARENT_NODE.RIGHT = DENOMINATOR
        End If

        'Third Case
        If NODE.VALUE = "/" And PARENT_NODE.VALUE = "*" Then
            PARENT_NODE.VALUE = "/"
            Dim CHILDREN_OF_PARENT As New List(Of TREE_NODE) ' I must get all the children except the child I have selected.
            For Each NODE_LOOP As TREE_NODE In PARENT_NODE.LEFT
                If Not NODE_LOOP Is NODE Then
                    CHILDREN_OF_PARENT.Add(NODE_LOOP)
                End If
            Next
            For Each NODE_LOOP As TREE_NODE In PARENT_NODE.RIGHT ' I keep right and left anyway, eventhough it is not needed.
                If Not NODE_LOOP Is NODE Then
                    CHILDREN_OF_PARENT.Add(NODE_LOOP)
                End If
            Next
            Dim PRODUCT_NODE As New TREE_NODE
            PRODUCT_NODE.VALUE = "*"
            PRODUCT_NODE.LEFT = NODE.LEFT
            PRODUCT_NODE.RIGHT = CHILDREN_OF_PARENT
            Dim HOLDER As New List(Of TREE_NODE)
            HOLDER.Add(PRODUCT_NODE)
            PARENT_NODE.LEFT = HOLDER
            PARENT_NODE.RIGHT = NODE.RIGHT
        End If
    End Sub

    Private Function RATIONAL_SIMPLIFICATION(NODE As TREE_NODE)
        ' This is a much more complicated function than the first two REMOVE_NEGATIVITY AND LEVEL_OPERATORS. 
        ' It contains 3 cases.
        ' The point of this is to make it impossible for a division node to have a child that is also a division node.
        ' This aims to make it so that there is only one division node as the root (in any instance there is a division node..!), and its children are multplicative nodes (if needed).

        ' The three cases are checks to transform this tree in traversal.
        ' The first check looks for when a division node's numerator is a division node.
        ' The second check is the same, but for when the division node is in the denominator.
        ' The third case is when a child of a multplicative node is a division node. Only the first division node in the children is used. 

        ' These three cases are the foundation of this function. 
        ' Postorder traversal will be used for this simplification due to its inherent left right design. 

        ' !!Please note that due to the nature of this algorithm it must be run multiple times to ensure the simplification is completed!!

        If Not NODE.LEFT Is Nothing Then
            For A As Integer = 0 To NODE.LEFT.Count() - 1 ' I am modifying the stream, so I have to do this loop method.
                If A <= NODE.LEFT.Count - 1 Then
                    If Not NODE.LEFT(A) Is Nothing Then ' Makes sure it exists.
                        Dim NODE_ELEMENT As TREE_NODE = NODE.LEFT(A)
                        Dim TYPE_RETURN As String = RATIONAL_SIMPLIFICATION(NODE_ELEMENT) ' Returns either a * or a /
                        If Not TYPE_RETURN Is Nothing Then
                            SUB_RATIONAL_FUNCTION(NODE, NODE_ELEMENT, ETYPE.LEFT) ' I send the current node and the child node that has been deemed possible to simplify.
                        End If
                    End If
                End If
            Next
        End If
        If Not NODE.RIGHT Is Nothing Then
            For A As Integer = 0 To NODE.RIGHT.Count() - 1
                If A <= NODE.LEFT.Count - 1 Then
                    If Not NODE.RIGHT(A) Is Nothing Then
                        Dim NODE_ELEMENT As TREE_NODE = NODE.RIGHT(A)
                        Dim TYPE_RETURN As String = RATIONAL_SIMPLIFICATION(NODE_ELEMENT) ' Returns either a * or a / (currently optional data that may be used in later stages)
                        If Not TYPE_RETURN Is Nothing Then ' This will only run if it returned
                            SUB_RATIONAL_FUNCTION(NODE, NODE_ELEMENT, ETYPE.RIGHT) ' As the required code is large I will seperate it in a new function
                        End If
                    End If
                End If
            Next
        End If

        If NODE.VALUE = "/" Or NODE.VALUE = "*" Then ' This will be used to identify when a child has a division or multiplication node.
            ' The general idea is that the traversal will bring us to the bottom of the left sub tree, and when it does it will return nothing. 
            ' The recursive stack will then be popped through subsequent 'upping'. In cases where a certain instance gets a returned value of a * or a /
            ' It will then do the checks for the cases.
            Return NODE.VALUE
        End If
        Return Nothing
    End Function


    Private Sub REMOVE_NEGATIVITY(NODE As TREE_NODE)

        ' This uses a post order traversal. Ie, I need to see each consitutent node in a root, right and left, and evaluate whether I can convert them.
        ' The simple requirement is such that their root is negative.
        ' In this case a new node is created such that it is the multiplication of -1 to the negative node.
        ' The negative root is then made positive and I continue the traversal. 
        If Not NODE.LEFT Is Nothing Then
            For Each NODE_ELEMENT As TREE_NODE In NODE.LEFT ' This loops through each node, resulting in the same for binary trees, but will also allows non binary trees in cases where its needed.
                REMOVE_NEGATIVITY(NODE_ELEMENT)
            Next
        End If
        If Not NODE.RIGHT Is Nothing Then
            For Each NODE_ELEMENT As TREE_NODE In NODE.RIGHT
                REMOVE_NEGATIVITY(NODE_ELEMENT)
            Next
        End If
        ' Recursively calls this. I will do the calculations here, as both the right and left have been evaluated.
        ' I assume the tree is in binary format. This algorithm must be used before any other for this to be true.
        If NODE.LEFT.Count = 1 And NODE.RIGHT.Count = 1 And NODE.VALUE = "-" Then ' There are two items and the root is a negative.
            Dim NEW_NODE, NEGATIVE_NODE As New TREE_NODE
            NEW_NODE.VALUE = "*"
            NEGATIVE_NODE.VALUE = "-1"
            NEW_NODE.LEFT.Add(NEGATIVE_NODE) ' Adds the negative multiplication
            NEW_NODE.RIGHT.Add(NODE.RIGHT.Item(0)) ' Adds the exisiting node.
            NODE.RIGHT.Remove(NODE.RIGHT.Item(0)) ' Removes the exisiting node
            NODE.RIGHT.Add(NEW_NODE)
            NODE.VALUE = "+" ' Sets the root as a plus. This should now be correctly optimised.
        End If
    End Sub



    Private Sub PREPARATION_BY_UNIVERSAL_RULING(NODE As TREE_NODE)
        PREPARATION_BY_TIMES_RULING(NODE)
        LEVEL_OPERATORS(NODE)
        PREPERATION_POWER_RULING(NODE)
    End Sub

    Private Sub PREPERATION_POWER_RULING(NODE As TREE_NODE)

        ' Does not assume binary tree.


        Dim ITERATION_ARRAY = {NODE.LEFT, NODE.RIGHT}

        If Not NODE.LEFT Is Nothing Then
            For Each NODE_ELEMENT As TREE_NODE In NODE.LEFT
                PREPERATION_POWER_RULING(NODE_ELEMENT)
            Next
        End If
        If Not NODE.RIGHT Is Nothing Then
            For Each NODE_ELEMENT As TREE_NODE In NODE.RIGHT
                PREPERATION_POWER_RULING(NODE_ELEMENT)
            Next
        End If
        If NODE.VALUE = "*" Then
            For Each ELEMENT As List(Of TREE_NODE) In ITERATION_ARRAY
                ' This means it may have variables in it. 
                ' I can modify it such that I loop through each of the variables, and if they have no left and right children, I can modify it to a power node.
                ' This works as I assume I will traverse each node, so I will be looking at all the children. (He says with mocking self-assurance.)
                Dim CLONED_NODES As New List(Of TREE_NODE)
                CLONED_NODES.AddRange(ELEMENT)
                For A As Integer = 0 To ELEMENT.Count - 1
                    If CLONED_NODES(A).VALUE <> "+" And CLONED_NODES(A).VALUE <> "-" And CLONED_NODES(A).VALUE <> "/" And CLONED_NODES(A).VALUE <> "*" And CLONED_NODES(A).VALUE <> "^" And Not IsNumeric(CLONED_NODES(A).VALUE) Then ' Its just a check that means its a variable. There are other ways. 

                        Dim NEW_NODE, ONE_NODE As New TREE_NODE
                        NEW_NODE.VALUE = "^"
                        ONE_NODE.VALUE = "1"
                        NEW_NODE.LEFT.Add(CLONED_NODES(A))
                        NEW_NODE.RIGHT.Add(ONE_NODE)
                        ELEMENT.Remove(CLONED_NODES(A))
                        ELEMENT.Add(NEW_NODE)
                    End If
                Next
            Next
        End If
    End Sub
    Private Sub PREPARATION_BY_TIMES_RULING(NODE As TREE_NODE)

        ' To easily accomodate for variables and coefficients, I'll change all the numericals to a*1. 
        ' This is so that functions like 'COLLECT_LIKE_TERMS' can function with numbers. It assumes everything is in the form a*b, where a is the coefficient, and b the variable.
        ' In this case the variable will be 1, and this is fine when comparisons are made as it will still proceed with summations.

        ' I will also change variables to all have powers. 
        ' Ie, x = x^1, but in tree form. 
        ' This is useful for algorithms, as I won't need to accomodate special cases and turn spaghetti into taglietti. (assume one is worse than the other)
        ' If you can think of a better name than I'm all ears. Unfortunately I don't have any.


        Dim ITERATION_ARRAY = {NODE.LEFT, NODE.RIGHT}

        If Not NODE.LEFT Is Nothing Then
            For Each NODE_ELEMENT As TREE_NODE In NODE.LEFT
                PREPARATION_BY_TIMES_RULING(NODE_ELEMENT)
            Next
        End If
        If Not NODE.RIGHT Is Nothing Then
            For Each NODE_ELEMENT As TREE_NODE In NODE.RIGHT
                PREPARATION_BY_TIMES_RULING(NODE_ELEMENT)
            Next
        End If
        If NODE.VALUE = "+" Then
            For Each ELEMENT As List(Of TREE_NODE) In ITERATION_ARRAY
                For A As Integer = 0 To ELEMENT.Count - 1
                    Dim CHECK As Dictionary(Of String, String) = MATCH_COLLECTION_TO_DICTIONARY(Regex.Matches(ELEMENT(A).VALUE, "[*,+,/,-]")) 'Looks for operators.
                    If (IsNumeric(ELEMENT(A).VALUE) Or CHECK.Count = 0 And ELEMENT(A).VALUE <> "0") Then ' Every single node is checked, so I won't need to loop through. 
                        Dim NEW_NODE, ONE_NODE As New TREE_NODE
                        NEW_NODE.VALUE = "*"
                        ONE_NODE.VALUE = "1"
                        NEW_NODE.RIGHT.Add(ONE_NODE) ' Adds the 1 multiplication
                        NEW_NODE.LEFT.Add(ELEMENT(A).CLONE) ' Adds the exisiting node.
                        ELEMENT.RemoveAt(A)
                        ELEMENT.Add(NEW_NODE)
                    End If
                Next
            Next
        End If
    End Sub
    Private Function LEVEL_OPERATORS(NODE As TREE_NODE)

        ' In cases where (3+3)+4 occur there will be a binary method of parsing. Ie, two root nodes to create the tree.
        ' This is not needed, as a plus can be done to many items at once. I can do 3 5 3 +, and it will not change anything.
        ' This concept is also true for multiplication.
        ' Therefore the simplifcation involves changing * and + into a single node with many children - as many as possible. 
        ' Negation is assumed to be removed, but for division we must be careful and ignore them. 

        ' I will be using postorder for this. (postorder works from the bottom to the top naturally, which I am fanciful towards. Dunno why.)

        ' The entire children, left and right, will be examined. I will then need to go 'up' a root, and see if that root is the same as my current one. 
        ' If there are two */+ then I will edit the tree to accomodate for only one root, and all of the children. 
        ' This is done recursively so by the end the entire tree should be simplified. 
        ' A~wala

        If Not NODE.LEFT Is Nothing Then
            For A As Integer = 0 To NODE.LEFT.Count() - 1
                If A < NODE.LEFT.Count() Then
                    Dim NODE_ELEMENT As TREE_NODE = NODE.LEFT(A)
                    Dim TYPE_RETURN As String = LEVEL_OPERATORS(NODE_ELEMENT) ' This is vital. It returns a +, * or nothing. #
                    'If its a + or a * that means that the child can be simplified, if this root node has the same root.
                    ' We have to do this as this is a recursive execution from bottom to up. 
                    ' It will only return this value when it can no longer traverse down, so that means I can edit from bottom to top. (duh that is how it works)
                    If TYPE_RETURN <> Nothing And TYPE_RETURN = NODE.VALUE Then
                        Dim TO_MOVE_ELEMENT_TOTAL As New List(Of TREE_NODE)
                        TO_MOVE_ELEMENT_TOTAL.AddRange(NODE_ELEMENT.RIGHT)
                        TO_MOVE_ELEMENT_TOTAL.AddRange(NODE_ELEMENT.LEFT)
                        NODE.LEFT.RemoveAt(A)
                        NODE.LEFT.AddRange(TO_MOVE_ELEMENT_TOTAL)
                    End If
                End If
            Next
        End If
        If Not NODE.RIGHT Is Nothing Then
            For a As Integer = 0 To NODE.RIGHT.Count() - 1
                If a < NODE.RIGHT.Count() Then
                    Dim NODE_ELEMENT As TREE_NODE = NODE.RIGHT(a)
                    Dim TYPE_RETURN As String = LEVEL_OPERATORS(NODE_ELEMENT) ' this is vital. it returns a +, * or nothing. #
                    If TYPE_RETURN <> Nothing And TYPE_RETURN = NODE.VALUE Then
                        Dim TO_MOVE_ELEMENT_TOTAL As New List(Of TREE_NODE)
                        TO_MOVE_ELEMENT_TOTAL.AddRange(NODE_ELEMENT.RIGHT)
                        TO_MOVE_ELEMENT_TOTAL.AddRange(NODE_ELEMENT.LEFT)
                        NODE.RIGHT.RemoveAt(a)
                        NODE.RIGHT.AddRange(TO_MOVE_ELEMENT_TOTAL)
                    End If
                End If
            Next
        End If

        If NODE.VALUE = "+" Or NODE.VALUE = "*" Then
            Return NODE.VALUE
        End If

        Return Nothing
    End Function

    Public Sub DIVISION_MULTIPLICATION(NODE As TREE_NODE)
        ' let (a^b)^c =a^(b*c)

        If NODE.VALUE = "^" Then
            ' "^" Nodes are binary, so I can easily check this condition.
            If NODE.LEFT(0).VALUE = "^" Then
                NODE.LEFT.Add(NODE.LEFT(0).LEFT(0)) ' I will add the "a" to the top most power node.

                Dim MULTIPLY_NODE_1, MULTIPLY_NODE_2, MULTIPLY_NODE_HOLD As New TREE_NODE

                MULTIPLY_NODE_1 = NODE.RIGHT(0).CLONE ' the c
                MULTIPLY_NODE_2 = NODE.LEFT(0).RIGHT(0).CLONE ' the b
                MULTIPLY_NODE_HOLD.VALUE = "*"
                MULTIPLY_NODE_HOLD.LEFT.Add(MULTIPLY_NODE_1)
                MULTIPLY_NODE_HOLD.RIGHT.Add(MULTIPLY_NODE_2) ' This forms b*c

                NODE.RIGHT.RemoveAt(0) ' removes the original c, so it is left as ((a^b)a)^
                NODE.RIGHT.Add(MULTIPLY_NODE_HOLD) ' add the multiply node, so ((a^b)a)^(b*c)

                NODE.LEFT.RemoveAt(0) ' Remove the original power node. ie the a^b in the left, so we get a^(b*c)
            End If
        End If


        If Not NODE.LEFT Is Nothing Then
            For Each NODE_ELEMENT As TREE_NODE In NODE.LEFT
                DIVISION_MULTIPLICATION(NODE_ELEMENT)
            Next
        End If
        If Not NODE.RIGHT Is Nothing Then
            For Each NODE_ELEMENT As TREE_NODE In NODE.RIGHT
                DIVISION_MULTIPLICATION(NODE_ELEMENT)
            Next
        End If

    End Sub

    Public Sub NEGATIVE_POWERS_TO_DIVISORS(NODE As TREE_NODE)

        ' When finished with differentiation, revoke the negative powers to division nodes, so that it is displayed properly
        ' Let a^-b = 1/a^b


        If Not NODE.LEFT Is Nothing Then
            For Each NODE_ELEMENT As TREE_NODE In NODE.LEFT
                NEGATIVE_POWERS_TO_DIVISORS(NODE_ELEMENT)
            Next
        End If
        If Not NODE.RIGHT Is Nothing Then
            For Each NODE_ELEMENT As TREE_NODE In NODE.RIGHT
                NEGATIVE_POWERS_TO_DIVISORS(NODE_ELEMENT)
            Next
        End If

        If NODE.VALUE = "^" Then
            If IsNumeric(NODE.RIGHT(0).VALUE) Then
                If Convert.ToInt32(NODE.RIGHT(0).VALUE) < 0 Then
                    NODE.RIGHT(0).VALUE = Convert.ToString(-Convert.ToInt32(NODE.RIGHT(0).VALUE))

                    Dim CLONED_NODE = NODE.CLONE
                    If CLONED_NODE.RIGHT(0).VALUE = "1" Then ' Remove the power node :OOOO
                        CLONED_NODE.VALUE = CLONED_NODE.LEFT(0).VALUE

                        CLONED_NODE.LEFT.RemoveAt(0) ' Remove the original stuff
                        CLONED_NODE.RIGHT.RemoveAt(0)

                        CLONED_NODE.LEFT.AddRange(NODE.LEFT(0).LEFT) ' Add the left and right lists of the left node. Ie the a of a^b
                        CLONED_NODE.RIGHT.AddRange(NODE.LEFT(0).RIGHT)
                    End If


                    NODE.VALUE = "/" ' Make this node a divisor

                    Dim ONE_NODE As New TREE_NODE
                    ONE_NODE.VALUE = "1" ' Create the One_Node

                    NODE.LEFT.RemoveAt(0) 'Clear the node
                    NODE.RIGHT.RemoveAt(0)

                    NODE.LEFT.Add(ONE_NODE) ' Add the 1
                    NODE.RIGHT.Add(CLONED_NODE) ' Add the modified node. Ie 1/(a^b). 
                End If
            End If
        End If
    End Sub

End Class



Module MODULE1

    Sub MAIN()
        While True
            Console.Write("DO THE INPUT:")
            Dim INPUT As String = Console.ReadLine()
            Dim SIMPLIFIED As New SIMPLE_SIMPLIFY(INPUT)
            Console.WriteLine("")
            Console.WriteLine(SIMPLIFIED.RESULT)
            Console.WriteLine("")
            Console.WriteLine("DIFFERENTIATE? (y/n)")
            Console.WriteLine("If it is multivariable, note that the evaluation is d(f(x,y,z...))/dx.")
            Console.WriteLine("Partial Derivatives that evaluate as ∂(f(x))/∂x *dx will become ∂(f(x))/∂x.")
            Console.WriteLine("Partial Derivatives of any other form will produce a ratio derivative in a product.")
            Console.WriteLine("- Such as ∂(f(x))/∂x * dy/dx")
            Console.WriteLine("This form is correct for all multivariable calculations, no matter the number of arguments.")
            Dim YES As String = Console.ReadLine
            If YES.ToLower = "y" Then
                SIMPLIFIED.DIFFERENTIATE()
                Console.WriteLine("")
                Console.WriteLine("d(" & INPUT & ")" & "/dx = " & SIMPLIFIED.RESULT)
                Console.WriteLine("")
            End If
            Console.WriteLine("Expand the brackets? (y/n)")
            YES = Console.ReadLine
            If YES.ToLower = "y" Then
                SIMPLIFIED.EXPAND()
                Console.WriteLine("")
                Console.WriteLine(SIMPLIFIED.RESULT)
                Console.WriteLine("")
            End If
        End While
    End Sub
End Module
