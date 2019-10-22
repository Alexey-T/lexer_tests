' Copyright 2008 ESRI
' 
' All rights reserved under the copyright laws of the United States
' and applicable international laws, treaties, and conventions.
' 
' You may freely redistribute and use this sample code, with or
' without modification, provided you include the original copyright
' notice and use restrictions.
' 
' See use restrictions at <your ArcGIS install location>/developerkit/userestrictions.txt.
' 

Imports Microsoft.VisualBasic
Imports System
Imports System.Collections.Generic
Imports System.Collections
Imports System.Runtime.InteropServices
Imports System.Windows.Forms
Imports ESRI.ArcGIS.Framework
Imports ESRI.ArcGIS.Editor
Imports ESRI.ArcGIS.Carto
Imports ESRI.ArcGIS.ArcMapUI
Imports ESRI.ArcGIS.Display
Imports ESRI.ArcGIS.Geodatabase
Imports ESRI.ArcGIS.esriSystem
Imports ESRI.ArcGIS.Geometry
Imports ESRI.ArcGIS.ADF.CATIDs

''' <summary>
''' CutWithoutSelection implements IEditTask and IEditTaskName. 
''' It determines which polygon features to cut from the target layer. 
''' This task makes it easier to cut many features in the target layer.
''' </summary>

<ComClass(CutWithoutSelection.ClassId, CutWithoutSelection.InterfaceId, CutWithoutSelection.EventsId), _
 ProgId("CutPolygonsTaskVBNet.CutWithoutSelection")> _
Public Class CutWithoutSelection : Implements IEditTask, IEditTaskName
#Region "COM Registration Function(s)"
  <ComRegisterFunction(), ComVisibleAttribute(False)> _
  Public Shared Sub RegisterFunction(ByVal registerType As Type)
    ' Required for ArcGIS Component Category Registrar support
    ArcGISCategoryRegistration(registerType)

    'Add any COM registration code after the ArcGISCategoryRegistration() call

  End Sub

  <ComUnregisterFunction(), ComVisibleAttribute(False)> _
  Public Shared Sub UnregisterFunction(ByVal registerType As Type)
    ' Required for ArcGIS Component Category Registrar support
    ArcGISCategoryUnregistration(registerType)

    'Add any COM unregistration code after the ArcGISCategoryUnregistration() call

  End Sub

#Region "ArcGIS Component Category Registrar generated code"
  ''' <summary>
  ''' Required method for ArcGIS Component Category registration -
  ''' Do not modify the contents of this method with the code editor.
  ''' </summary>
  Private Shared Sub ArcGISCategoryRegistration(ByVal registerType As Type)
    Dim regKey As String = String.Format("HKEY_CLASSES_ROOT\CLSID\{{{0}}}", registerType.GUID)
    EditTasks.Register(regKey)

  End Sub
  ''' <summary>
  ''' Required method for ArcGIS Component Category unregistration -
  ''' Do not modify the contents of this method with the code editor.
  ''' </summary>
  Private Shared Sub ArcGISCategoryUnregistration(ByVal registerType As Type)
    Dim regKey As String = String.Format("HKEY_CLASSES_ROOT\CLSID\{{{0}}}", registerType.GUID)
    EditTasks.Unregister(regKey)

  End Sub

#End Region
#End Region

#Region "COM GUIDs"
  ' These  GUIDs provide the COM identity for this class 
  ' and its COM interfaces. If you change them, existing 
  ' clients will no longer be able to access the class.
  Public Const ClassId As String = "eb1e08bc-ed97-4b68-90b0-b1067a9049a0"
  Public Const InterfaceId As String = "5a01d862-ace1-4f19-ad5f-6f18e85f986f"
  Public Const EventsId As String = "f3b36a19-53ba-4b57-8a8b-9f72438f8669"
#End Region

  Private m_editor As IEditor
  Private m_editSketch As IEditSketch
  Private m_editLayer As IEditLayers
  Private m_mxDoc As IMxDocument

#Region "IEditTask Members"

  Private Sub Activate(ByVal Editor As IEditor, ByVal oldTask As IEditTask) Implements IEditTask.Activate
    If Editor Is Nothing Then
      Return
    End If

    'Initialize class member variables.
    m_editor = Editor
    m_editSketch = TryCast(Editor, IEditSketch)
    m_editSketch.GeometryType = esriGeometryType.esriGeometryPolyline
    m_editLayer = TryCast(Editor, IEditLayers)
    m_mxDoc = TryCast(Editor.Parent.Document, IMxDocument)

    'Wire editor events.
    AddHandler (CType(m_editor, IEditEvents_Event)).OnCurrentLayerChanged, AddressOf OnCurrentLayerChanged
  End Sub

  Private Sub OnCurrentLayerChanged()
    If m_editLayer Is Nothing Then
      Return
    End If

    'Only enable the sketch tool if there is a polygon target layer.
    If m_editLayer.CurrentLayer.FeatureClass.ShapeType <> esriGeometryType.esriGeometryPolygon Then
      m_editSketch.GeometryType = esriGeometryType.esriGeometryNull
    Else
      'Set the edit sketch geometry type to be esriGeometryPolyline.
      m_editSketch.GeometryType = esriGeometryType.esriGeometryPolyline
    End If
  End Sub

  Private Sub Deactivate() Implements IEditTask.Deactivate
    'Stop listening for editor events.
    RemoveHandler (CType(m_editor, IEditEvents_Event)).OnCurrentLayerChanged, AddressOf OnCurrentLayerChanged

    'Release object references.
    m_editor = Nothing
    m_editSketch = Nothing
    m_editLayer = Nothing
    m_mxDoc = Nothing
  End Sub

  Private ReadOnly Property Name() As String Implements IEditTask.Name
    Get
      Return "Cut Polygons Without Selection_VBNet"
    End Get
  End Property

  Private Sub OnDeleteSketch() Implements IEditTask.OnDeleteSketch
  End Sub

  Private Sub OnFinishSketch() Implements IEditTask.OnFinishSketch
    If m_editSketch Is Nothing Then
      Return
    End If

    Dim HasCutPolygons As Boolean = False

    'Change the cursor to be hourglass shape.
    System.Windows.Forms.Cursor.Current = Cursors.WaitCursor

    Try
      'Get the geometry that performs the cut from the edit sketch.
      Dim cutGeometry As IGeometry = m_editSketch.Geometry

      'The sketch geometry is simplified to deal with a multi-part sketch as well
      'as the case where the sketch loops back over itself.
      Dim topoOperator As ITopologicalOperator2 = TryCast(cutGeometry, ITopologicalOperator2)
      topoOperator.IsKnownSimple_2 = False
      topoOperator.Simplify()

      'Get ready to invalidate the features.
      Dim refreshArea As IInvalidArea = New InvalidAreaClass()
      refreshArea.Add(cutGeometry.Envelope)

      'Create the spatial filter to search for features in the target feature class.
      'The spatial relationship we care about is whether the interior of the line 
      'intesects the interior of the polygon.
      Dim spatialFilter As ISpatialFilter = New SpatialFilterClass()
      spatialFilter.Geometry = m_editSketch.Geometry
      spatialFilter.SpatialRel = esriSpatialRelEnum.esriSpatialRelIntersects

      'Find the polygon features that cross the sketch.
      Dim featureClass As IFeatureClass = m_editLayer.CurrentLayer.FeatureClass
      Dim featureCursor As IFeatureCursor = featureClass.Search(spatialFilter, False)

      'Only do work if there are features that intersect the edit sketch.
      Dim origFeature As IFeature = featureCursor.NextFeature()
      If Not origFeature Is Nothing Then
        'Check the first feature to see if it is ZAware and if it needs to make the
        'cut geometry ZAware.
        Dim zAware As IZAware = TryCast(origFeature.Shape, IZAware)
        If zAware.ZAware Then
          zAware = TryCast(cutGeometry, IZAware)
          zAware.ZAware = True
        End If

        'The result selection set will be the new features created. This means 
        'we need to add the currently selected features to the refresh area so 
        'they are correctly invalidated.
        If m_editor.SelectionCount > 0 Then
          Dim selectedFeatures As IEnumFeature = m_editor.EditSelection
          selectedFeatures.Reset()
          Dim selectedFeature As IFeature = selectedFeatures.Next()
          Do While Not selectedFeature Is Nothing
            refreshArea.Add(selectedFeature)
            selectedFeature = selectedFeatures.Next()
          Loop
        End If

        'Initialize the selection set for the new features.
        Dim featureSelect As IFeatureSelection = TryCast(m_editLayer.CurrentLayer, IFeatureSelection)
        featureSelect.Clear()
        'Cache the new features created.
        Dim selectionSet As ISelectionSet = featureSelect.SelectionSet

        'Start an edit operation so we can have undo/redo.
        m_editor.StartOperation()

        'Cycle through the features, cutting with the sketch.
        Do While Not origFeature Is Nothing

          Try
            'Add the feature to the invalidate object.
            refreshArea.Add(origFeature)

            'Split the feature. Use the IFeatureEdit::Split method which ensures
            'the attributes are correctly dealt with.
            Dim featureEdit As IFeatureEdit = TryCast(origFeature, IFeatureEdit)
            'Set to hold the new features that are created by the Split.            
            Dim newFeaturesSet As ISet = featureEdit.Split(cutGeometry)

            'New features have been created, loop through the set and flash 
            'each feature and get it's OID for the final selection set.
            If Not newFeaturesSet Is Nothing Then
              newFeaturesSet.Reset()

              'A list to hold the selected features' OIDs.
              Dim iOIDList As List(Of Integer) = New List(Of Integer)()

              Dim featureCount As Integer = 0
              Do While featureCount < newFeaturesSet.Count
                HasCutPolygons = True
                'Flash the new features.
                Dim newFeature As IFeature = TryCast(newFeaturesSet.Next(), IFeature)
                FlashGeometry(newFeature.Shape, m_editor.Display)

                'Add the feature to the new selection set
                iOIDList.Add(newFeature.OID)
                featureCount += 1
              Loop
              Dim iOIDArray As Integer() = iOIDList.ToArray()

              selectionSet.AddList(newFeaturesSet.Count, iOIDArray(0))
            End If
          Catch e1 As COMException
          Finally
            'Continue to work on the next feature if it fails to split the current one.
            origFeature = featureCursor.NextFeature()
          End Try
        Loop
        'If any polygons were cut, invalidate the display and stop the edit operation.
        If HasCutPolygons Then
          'Clear the map's selection.
          m_mxDoc.FocusMap.ClearSelection()

          'Select the new features.
          featureSelect.SelectionSet = selectionSet

          'Refresh the display.
          refreshArea.Display = m_editor.Display
          refreshArea.Invalidate(CShort(esriScreenCache.esriAllScreenCaches))

          'Complete the edit operation.
          m_editor.StopOperation("Cut Polygons Without Selection")

          'As we changed the selection set, the map has no idea we did this.
          'Fire the event to inform others the selection has changed.
          featureSelect.SelectionChanged()
        Else
          m_editor.AbortOperation()
        End If
      End If
    Catch e As Exception
      MessageBox.Show("Unable to perform the cut task." & Constants.vbLf + e.ToString())
      'In the event of an error, abort the operation.
      m_editor.AbortOperation()
    Finally
      'Change the cursor shape to default.
      System.Windows.Forms.Cursor.Current = Cursors.Default
    End Try
  End Sub

  Private Sub FlashGeometry(ByVal geo As IGeometry, ByVal display As IDisplay)
    'Flash the input polygon geometry.
    display.StartDrawing(display.hDC, CShort(esriScreenCache.esriNoScreenCache))

    'Time in milliseconds to wait.
    Dim interval As Integer = 150
    Select Case geo.GeometryType
      Case esriGeometryType.esriGeometryPolygon
        'Set the flash geometry's symbol.
        Dim color As IRgbColor = New RgbColorClass()
        color.Red = 255

        Dim simpleFillSymbol As ISimpleFillSymbol = New SimpleFillSymbolClass()
        simpleFillSymbol.Color = color

        Dim symbol As ISymbol = TryCast(simpleFillSymbol, ISymbol)
        symbol.ROP2 = esriRasterOpCode.esriROPNotXOrPen

        display.SetSymbol(symbol)
        display.DrawPolygon(geo)
        System.Threading.Thread.Sleep(interval)
        display.DrawPolygon(geo)

    End Select
    display.FinishDrawing()
  End Sub

#End Region

#Region "IEditTaskName Members"

  Private ReadOnly Property UniqueName() As String Implements IEditTaskName.UniqueName
    'Allow users to uniquely identify the edit task by name.
    Get
      Return "CutPolygonsWithoutSelectionTask"
    End Get
  End Property

#End Region


End Class
