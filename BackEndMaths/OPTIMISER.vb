Imports System.Text.RegularExpressions

Enum ETYPE
    LEFT
    RIGHT
End Enum


Class OPTIMISER : Inherits UTILITIES

    ' General class for optimising trees. 
    Private TREE_TO_MODIFY As TREE_NODE



    Public Function OPTIMISE_TREE(TO_MODIFY As TREE_NODE)
        ' This function is important in simplifying the tree expressions. The disadvantage of trees are such that certain multiplicative expressions can be interpreted in multiple ways. 
        ' Division and Negative nodes are not optimal for a computer to handle. This function/(collection of functions) aims to solve these issues by rearranging nodes into a form that is fundamentally the same.
        ' ... But much easier to interpret for a tree. 

        ' Change the negative nodes to positive nodes, where the negative value is now a multiply node with -1 and the actual value.
        ' Level operators. Operators like + and * can be changed to have many children, instead of the binary two. This is why removing - and simplifying / operators are important.
        Dim LAST_TREE As String
        Console.WriteLine("FIRST BOI")
        Console.WriteLine(IN_ORDER(TO_MODIFY, True))
        TREE_TO_MODIFY = TO_MODIFY
        PREPARATION_BY_UNIVERSAL_RULING(TREE_TO_MODIFY)
        While Not LAST_TREE = IN_ORDER(TREE_TO_MODIFY, True)
            LAST_TREE = IN_ORDER(TREE_TO_MODIFY, True)
            REMOVE_NEGATIVITY(TREE_TO_MODIFY) ' Removes the negative roots.
            TREE_TO_MODIFY = TO_MODIFY
            LEVEL_OPERATORS(TREE_TO_MODIFY) ' levels the operators, see function for more details.
            RATIONAL_SIMPLIFICATION(TREE_TO_MODIFY)
            Console.WriteLine("FINISHED BOI")
            Console.WriteLine(IN_ORDER(TREE_TO_MODIFY, True))
        End While
        LAST_TREE = ""
        While Not LAST_TREE = IN_ORDER(TREE_TO_MODIFY, True)
            LAST_TREE = IN_ORDER(TREE_TO_MODIFY, True)
            COLLECT_LIKE_TERMS(TREE_TO_MODIFY)
            Console.WriteLine("FINISHED BOI V2")
            Console.WriteLine(IN_ORDER(TREE_TO_MODIFY, True))
        End While


        ' Those optimisations can be considered 'trivial' based on the fact that they merely reorganise the expression. 
        ' I now need to collect like terms, simplify trivial terms like x^0 = 1, and distribute (a+b)(c+d).
        ' These are supposedly more complicated.



        Return True
    End Function


    ' Simplification Functions
    ' //These are made to actually modify the expression.
    '//// THIS TO BE FINISHED!! ////  TAKE INTO ACCOUNT NEAGTIVE NUMBERS!


    Private Sub SIMPLE_COLLECT_FRACTION_DENOMINATORS(NODE As TREE_NODE)
        ' This is made to combine fractions with the same denominator.
        ' It should be a fairly simple scenarior to solve, and the reason for its existence is for the like term collector to work properly.
        ' There are a lot more nuances to this solution, though, so I named this one 'Simple'.
        ' A 'common denominator' can normally be found with ease.
        ' a/b + c/d, I need to multiply by every other denominator for each term. a/b * d/d. c/d * b/b. 

        ' This function was made before any Multiplication Solver was implemented, so it is crippled in functionality.
        ' I do not intend to make a fully featured simplifier, so this is fine.
        ' ~I'll leave it at that before I enter a self serving tangent and realise the enormity of what I'm attempting to 'partially' solve.





    End Sub

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
        If NODE.VALUE = "+" Then
            Console.WriteLine("PARENT" & NODE.VALUE)
        End If
        ' I consider this an 'Up to Down' Method.

        If NODE.VALUE = "+" Then ' Like terms can be collected.
            For Each ELEMENT As List(Of TREE_NODE) In ITERATION_ARRAY ' This loops through the 
                Console.WriteLine("COUNTTT" & ELEMENT.Count)
                For A As Integer = 0 To ELEMENT.Count() - 1 ' I am modifying the table as I loop, so this is a requirement. Either that, or cloning the table.
                    If (A) <= (ELEMENT.Count - 1) Then
                        Console.WriteLine("LOOPING")
                        Dim ELEMENT_A As TREE_NODE = ELEMENT(A) ' The element I will compare against all the others within the NODE_LIST.
                        If ELEMENT_A.VALUE <> "+" Then
                            Dim ELEMENT_TOTAL, ELEMENT_B As TREE_NODE
                            ' Console.WriteLine(IN_ORDER(ELEMENT_A, True) & " juh ")
                            Console.WriteLine("NODE VALUE" & ELEMENT_A.VALUE)
                            For Each ELEMENT_B_TEMP As TREE_NODE In NODE_LIST ' I will now compare it to every node I HAVE looked at. (This is better than, say, comparing it to everything from the getgo.)
                                Dim TERMS_A, TERMS_B As SEPERATED_TERMS
                                Console.WriteLine("WHAT THE FUCK IS THIS " & IN_ORDER(ELEMENT_B_TEMP, True))
                                TERMS_A = RETURN_VARIABLE_AND_COEFFICIENT(ELEMENT_A.CLONE())
                                TERMS_B = RETURN_VARIABLE_AND_COEFFICIENT(ELEMENT_B_TEMP.CLONE())
                                '  Console.WriteLine(IN_ORDER(ELEMENT_A, True) & " WHAT " & IN_ORDER(ELEMENT_B_TEMP, True))
                                Console.WriteLine(TERMS_A.COEFFICIENT & " SByte " & IN_ORDER(TERMS_A.VARIABLE, True) & "COFF" & IN_ORDER(TERMS_B.VARIABLE, True) & " SByte " & TERMS_B.COEFFICIENT)
                                If IN_ORDER(TERMS_A.VARIABLE, True) = IN_ORDER(TERMS_B.VARIABLE, True) Then
                                    Dim NEW_NODE As New TREE_NODE
                                    NEW_NODE.VALUE = TERMS_A.COEFFICIENT + TERMS_B.COEFFICIENT ' This will be the new coefficient
                                    Console.WriteLine("NEW COFF" & NEW_NODE.VALUE)
                                    ' Console.WriteLine("FINALISED THING" & IN_ORDER(TERMS_B.VARIABLE, True) & "NODE ROOT" & TERMS_B.VARIABLE.LEFT(0).VALUE)
                                    If TERMS_B.VARIABLE.VALUE = "*" Then
                                        TERMS_B.VARIABLE.LEFT.Insert(0, NEW_NODE) ' I will add the coefficient to the front of the node's variable. 
                                    ElseIf TERMS_B.VARIABLE.VALUE = "/" Then ' As "/" is binary, then I must insert it into the extra node beyond the left node. Say * or +. Ie, its some addition, or multiplication, divided by something else.
                                        TERMS_B.VARIABLE.LEFT(0).LEFT.Insert(0, NEW_NODE) ' I'll go left in the left because who likes it right.
                                    End If
                                    ELEMENT_TOTAL = TERMS_B.VARIABLE ' This will be the new node.
                                    ELEMENT_B = ELEMENT_B_TEMP ' This is here so I can remove it.
                                    Exit For
                                End If
                            Next

                            If Not ELEMENT_B Is Nothing Then ' As it is an 'each' loop, I will need to modify it outside of it. And yes I hate the 'IsNot Nothing'
                                Console.WriteLine(IN_ORDER(ELEMENT_A, True) & " the fukc" & IN_ORDER(ELEMENT_B, True))
                                NODE_LIST.Remove(ELEMENT_B)
                                If ELEMENT.Contains(ELEMENT_B) Then
                                    ELEMENT.Remove(ELEMENT_B)
                                End If
                                ITERATION_ARRAY(1).Remove(ELEMENT_B) ' "It just works." TM
                                ITERATION_ARRAY(0).Remove(ELEMENT_B) ' If the list becomes massive then this will become a problem. It won't become massive.
                                ELEMENT.Remove(ELEMENT_A)
                                ELEMENT.Add(ELEMENT_TOTAL)
                            Else ' This means no like term could be found. Thus I will just add ELEMENT_A to the NODE_LIST to be compared to the rest.
                                Console.WriteLine("ADFDING TO NODE LIST BAHAHA" & ELEMENT_A.VALUE & " actual THING" & IN_ORDER(ELEMENT_A, True))
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
            Dim BACK_TRACK_B As Integer = 0
            For A As Integer = 0 To NODE.LEFT.Count() - 1
                COLLECT_LIKE_TERMS(NODE.LEFT(A)) 'A simple traversal, which is nice.
            Next
        End If

        If Not NODE.RIGHT Is Nothing Then
            Dim BACK_TRACK_B As Integer = 0
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
        Dim COEFFICIENT As Integer = 0
        Dim RETURN_STRUCTURE As New SEPERATED_TERMS

        'Console.WriteLine("THIS IS WHAT WILL BE SOPRTED " & IN_ORDER(NODE, True))

        If Not NODE.LEFT Is Nothing Then
            For A As Integer = 0 To NODE.LEFT.Count() - 1
                If A <= (NODE.LEFT.Count - 1) And NODE.VALUE <> "^" Then ' Makes sure it exists.
                    Dim NODE_ELEMENT_S As SEPERATED_TERMS = RETURN_VARIABLE_AND_COEFFICIENT(NODE.LEFT(A))
                    Dim NODE_ELEMENT As TREE_NODE = NODE_ELEMENT_S.VARIABLE
                    COEFFICIENT += NODE_ELEMENT_S.COEFFICIENT
                    If IsNumeric(NODE_ELEMENT.VALUE) Then
                        NODE.LEFT.RemoveAt(A) ' Removes the coefficient
                        COEFFICIENT = COEFFICIENT + NODE_ELEMENT.VALUE
                        ' Console.WriteLine("addd 1" & COEFFICIENT)
                    End If
                    If NODE_ELEMENT.RIGHT.Count = 1 And NODE_ELEMENT.LEFT.Count = 1 Then
                        If NODE_ELEMENT.VALUE = "*" And NODE_ELEMENT.LEFT(0).VALUE = "-1" Then ' This is a negative number
                            NODE.LEFT.RemoveAt(A) ' Removes the coefficient
                            COEFFICIENT = COEFFICIENT - NODE_ELEMENT.RIGHT(0).VALUE
                            'Console.WriteLine("addd 2" & COEFFICIENT)
                        End If
                    End If
                End If
            Next
        End If

        If Not NODE.RIGHT Is Nothing Then
            For A As Integer = 0 To NODE.RIGHT.Count() - 1
                If NODE.RIGHT.Count = 1 And IsNumeric(NODE.RIGHT(0).VALUE) Then ' This takes into account that a number will be in the form (a*b, where a is the coefficient and b "1"
                    Exit For
                End If
                If A <= (NODE.RIGHT.Count - 1) And NODE.VALUE <> "^" Then  ' Makes sure it exists.
                    Dim NODE_ELEMENT_S As SEPERATED_TERMS = RETURN_VARIABLE_AND_COEFFICIENT(NODE.RIGHT(A))
                    Dim NODE_ELEMENT As TREE_NODE = NODE_ELEMENT_S.VARIABLE
                    COEFFICIENT += NODE_ELEMENT_S.COEFFICIENT
                    If IsNumeric(NODE_ELEMENT.VALUE) Then
                        NODE.RIGHT.RemoveAt(A) ' Removes the coefficient
                        COEFFICIENT = COEFFICIENT + NODE_ELEMENT.VALUE
                    End If
                    If NODE_ELEMENT.RIGHT.Count = 1 And NODE_ELEMENT.LEFT.Count = 1 Then
                        If NODE_ELEMENT.VALUE = "*" And NODE_ELEMENT.LEFT(0).VALUE = "-1" Then ' This is a negative number
                            NODE.RIGHT.RemoveAt(A) ' Removes the coefficient
                            COEFFICIENT = COEFFICIENT - NODE_ELEMENT.RIGHT(0).VALUE
                        End If
                    End If
                End If
            Next
        End If

        'Console.WriteLine("RETURNED COF " & COEFFICIENT)
        Console.WriteLine("addd" & COEFFICIENT)
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
                If Not NODE.LEFT(A) Is Nothing Then ' Makes sure it exists.
                    Dim NODE_ELEMENT As TREE_NODE = NODE.LEFT(A)
                    Dim TYPE_RETURN As String = RATIONAL_SIMPLIFICATION(NODE_ELEMENT) ' Returns either a * or a /
                    If Not TYPE_RETURN Is Nothing Then
                        SUB_RATIONAL_FUNCTION(NODE, NODE_ELEMENT, ETYPE.LEFT) ' I send the current node and the child node that has been deemed possible to simplify.
                    End If
                End If
            Next
        End If
        If Not NODE.RIGHT Is Nothing Then
            For A As Integer = 0 To NODE.RIGHT.Count() - 1
                If Not NODE.RIGHT(A) Is Nothing Then
                    Dim NODE_ELEMENT As TREE_NODE = NODE.RIGHT(A)
                    Dim TYPE_RETURN As String = RATIONAL_SIMPLIFICATION(NODE_ELEMENT) ' Returns either a * or a / (currently optional data that may be used in later stages)
                    If Not TYPE_RETURN Is Nothing Then ' This will only run if it returned
                        SUB_RATIONAL_FUNCTION(NODE, NODE_ELEMENT, ETYPE.RIGHT) ' As the required code is large I will seperate it in a new function
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

        ' This requires a post order traversal. Ie, I need to see each consitutent node in a root, right and left, and evaluate whether I can convert them.
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
        '!!This assumes it IS a Binary Tree!!

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
                PREPARATION_BY_UNIVERSAL_RULING(NODE_ELEMENT)
            Next
        End If
        If Not NODE.RIGHT Is Nothing Then
            For Each NODE_ELEMENT As TREE_NODE In NODE.RIGHT
                PREPARATION_BY_UNIVERSAL_RULING(NODE_ELEMENT)
            Next
        End If

        For Each ELEMENT As List(Of TREE_NODE) In ITERATION_ARRAY
            If ELEMENT.Count > 0 Then
                Console.WriteLine("you fucking twat" & ELEMENT(0).VALUE)
                If IsNumeric(ELEMENT(0).VALUE) Then ' Every single node is checked, so I won't need to loop through. 
                    Dim NEW_NODE, ONE_NODE As New TREE_NODE
                    NEW_NODE.VALUE = "*"
                    ONE_NODE.VALUE = "1"
                    NEW_NODE.RIGHT.Add(ONE_NODE) ' Adds the 1 multiplication
                    NEW_NODE.LEFT.Add(ELEMENT(0)) ' Adds the exisiting node.
                    ELEMENT.RemoveAt(0)
                    ELEMENT.Add(NEW_NODE)
                End If
                If NODE.VALUE = "*" Then
                    Console.WriteLine("THERES A FEKING TIMES")
                    ' This means it may have variables in it. 
                    ' I can modify it such that I loop through each of the variables, and if they have no left and right children, I can modify it to a power node.
                    ' This works as I assume I will traverse each node, so I will be looking at all the children. (He says with mocking self-assurance.)
                    For A As Integer = 0 To ELEMENT.Count - 1
                        Console.WriteLine(ELEMENT(0).VALUE & " lol")
                        If ELEMENT(A).VALUE <> "+" And ELEMENT(A).VALUE <> "-" And ELEMENT(A).VALUE <> "/" And ELEMENT(A).VALUE <> "*" And ELEMENT(A).VALUE <> "^" And Not IsNumeric(ELEMENT(A).VALUE) Then ' Its just a check that means its a variable. There are other ways. 
                            Dim NEW_NODE, ONE_NODE As New TREE_NODE
                            NEW_NODE.VALUE = "^"
                            ONE_NODE.VALUE = "1"
                            NEW_NODE.LEFT.Add(ELEMENT(0))
                            NEW_NODE.RIGHT.Add(ONE_NODE)
                            ELEMENT.RemoveAt(0)
                            ELEMENT.Add(NEW_NODE)
                        End If
                    Next
                End If
            End If
        Next
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
                Dim NODE_ELEMENT As TREE_NODE = NODE.LEFT(A)
                Dim TYPE_RETURN As String = LEVEL_OPERATORS(NODE_ELEMENT) ' This is vital. It returns a +, * or nothing. #
                'If its a + or a * that means that the child can be simplified, if this root node has the same root.
                ' We have to do this as this is a recursive execution from bottom to up. 
                ' It will only return this value when it can no longer traverse down, so that means I can edit from bottom to top. (duh that is how it works)
                If TYPE_RETURN <> Nothing And TYPE_RETURN = NODE.VALUE Then
                    Dim TO_MOVE_ELEMENT_TOTAL As List(Of TREE_NODE) = NODE_ELEMENT.LEFT
                    Dim TO_MOVE_ELEMENT_SUB As List(Of TREE_NODE) = NODE_ELEMENT.RIGHT
                    TO_MOVE_ELEMENT_TOTAL.AddRange(TO_MOVE_ELEMENT_SUB)
                    NODE.LEFT = TO_MOVE_ELEMENT_TOTAL
                End If
            Next
        End If
        If Not NODE.RIGHT Is Nothing Then
            For A As Integer = 0 To NODE.RIGHT.Count() - 1
                Dim NODE_ELEMENT As TREE_NODE = NODE.RIGHT(A)
                Dim TYPE_RETURN As String = LEVEL_OPERATORS(NODE_ELEMENT) ' This is vital. It returns a +, * or nothing. #
                If TYPE_RETURN <> Nothing And TYPE_RETURN = NODE.VALUE Then
                    Dim TO_MOVE_ELEMENT_TOTAL As List(Of TREE_NODE) = NODE_ELEMENT.LEFT
                    Dim TO_MOVE_ELEMENT_SUB As List(Of TREE_NODE) = NODE_ELEMENT.RIGHT
                    TO_MOVE_ELEMENT_TOTAL.AddRange(TO_MOVE_ELEMENT_SUB) ' Joins the two lists. Remember that each left and right are of the childs left and right. The whole parent is the right.
                    NODE.RIGHT = TO_MOVE_ELEMENT_TOTAL
                End If
            Next
        End If

        If NODE.VALUE = "+" Or NODE.VALUE = "*" Then
            Return NODE.VALUE
        End If

        Return Nothing
    End Function

End Class


Module MODULE1

    Sub MAIN()
        Dim SIMPLIFIED As New SIMPLE_SIMPLIFY("(9x^2+10x^2+9yx^(5x+2)+99yx^(5x+2))/10+9x/10")
        Console.WriteLine("SUM" & SIMPLIFIED.RESULT)
        Console.Read()
        Console.ReadKey()
    End Sub
End Module
