Imports System.Text.RegularExpressions
' A commonly used collection of methods.
Class UTILITIES

    ' For Trees

    Public Function IN_ORDER(NODE As TREE_NODE, FIRST As Boolean) ' Returns an inorder string.
        Dim OUTPUT As String = "" ' String to be created.
        Dim Count As Integer = 0
        If FIRST = False And NODE.LEFT.Count > 0 And NODE.RIGHT.Count > 0 Then
            OUTPUT = OUTPUT & "("
        End If
        If Not NODE.LEFT Is Nothing Then
            For Each NODE_ELEMENT As TREE_NODE In NODE.LEFT ' This loops through each node, resulting in the same for binary trees, but will also allows non binary trees in cases where its needed.
                Count += 1
                If Count > 1 Then
                    OUTPUT = OUTPUT & NODE.VALUE
                End If
                OUTPUT = OUTPUT & IN_ORDER(NODE_ELEMENT, False)
            Next
        End If
        OUTPUT = OUTPUT & NODE.VALUE
        If Not NODE.RIGHT Is Nothing Then
            Count = 0
            For Each NODE_ELEMENT As TREE_NODE In NODE.RIGHT
                Count += 1
                If Count > 1 Then
                    OUTPUT = OUTPUT & NODE.VALUE
                End If
                OUTPUT = OUTPUT & IN_ORDER(NODE_ELEMENT, False)
            Next
        End If
        If FIRST = False And NODE.LEFT.Count > 0 And NODE.RIGHT.Count > 0 Then
            OUTPUT = OUTPUT & ")"
        End If
        Return OUTPUT
    End Function

    ' For Linear Data Structures

    Public Function MATCH_COLLECTION_TO_DICTIONARY(INPUT As MatchCollection) ' Converts a match collection to dictionary.
        Dim TO_OUTPUT As New Dictionary(Of String, String)
        For Each ELEMENT As Match In INPUT
            If ELEMENT.Value <> "" And ELEMENT.Value <> "0" Then ' For some reason some regex gives "" in some areas.  
                TO_OUTPUT.Add(ELEMENT.Value, ELEMENT.Value)
            End If
        Next
        Return TO_OUTPUT ' Returns the collected dictionary
    End Function

    Public Function MATCH_COLLECTION_TO_STRING(INPUT As MatchCollection) ' Converts a match collection to a string.
        Dim TO_OUTPUT As String = ""
        For Each ELEMENT As Match In INPUT
            TO_OUTPUT = TO_OUTPUT & ELEMENT.Value
        Next
        Return TO_OUTPUT ' Returns the collected string
    End Function

    Public Function QUEUE_TO_STRING(INPUT As Queue(Of String))
        Dim CREATED_STRING As String = "" ' The created string.
        For Each CHAR_LIST As String In INPUT ' Goes through each char and appends them together
            CREATED_STRING = CREATED_STRING & CHAR_LIST
        Next
        Return CREATED_STRING
    End Function

    Public Function DICTIONARY_TO_STRING(INPUT As Dictionary(Of String, String))
        Dim RETURN_STRING As String = ""
        For Each ELEMENT As KeyValuePair(Of String, String) In INPUT
            Console.WriteLine(ELEMENT.Value)
            RETURN_STRING = RETURN_STRING & ELEMENT.Value
        Next
        Return RETURN_STRING
    End Function
    Public Function DICTIONARY_KEYS(INPUT As Dictionary(Of String, String))
        Dim RETURN_STRING As List(Of String) = New List(Of String)
        For Each ELEMENT As KeyValuePair(Of String, String) In INPUT
            RETURN_STRING.Add(ELEMENT.Key)
        Next
        Return RETURN_STRING
    End Function
End Class

