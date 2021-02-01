Imports System.IO
Imports System.Collections
Imports System.Runtime.Serialization.Formatters.Binary
Imports System.Runtime.Serialization
<Serializable()> Class TREE_NODE ' This is a general tree with n children for each node. It is partitioned into left and right to allow for inorder traversing.
    Public VALUE As String
    Public LEFT As New List(Of TREE_NODE)
    Public RIGHT As New List(Of TREE_NODE)

    Function CLONE() As TREE_NODE ' The only reason this exists is because of one function.

        ' If the object is nil then return nothing
        If (Object.ReferenceEquals(Me, Nothing)) Then Return Nothing

        Dim FORM As New BinaryFormatter()
        Dim STREAM As New MemoryStream()

        FORM.Serialize(STREAM, Me)
        STREAM.Seek(0, SeekOrigin.Begin) ' clones the object... deep.

        Return CType(FORM.Deserialize(STREAM), TREE_NODE) ' Returns the object in proper form.

    End Function
End Class

