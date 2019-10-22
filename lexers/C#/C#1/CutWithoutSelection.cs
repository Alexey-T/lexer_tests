// Copyright 2008 ESRI
// 
// All rights reserved under the copyright laws of the United States
// and applicable international laws, treaties, and conventions.
// 
// You may freely redistribute and use this sample code, with or
// without modification, provided you include the original copyright
// notice and use restrictions.
// 
// See use restrictions at <your ArcGIS install location>/developerkit/userestrictions.txt.
// 

using System;
using System.Collections.Generic;
using System.Collections;
using System.Runtime.InteropServices;
using System.Windows.Forms;
using ESRI.ArcGIS.Framework;
using ESRI.ArcGIS.Editor;
using ESRI.ArcGIS.Carto;
using ESRI.ArcGIS.ArcMapUI;
using ESRI.ArcGIS.Display;
using ESRI.ArcGIS.Geodatabase;
using ESRI.ArcGIS.esriSystem;
using ESRI.ArcGIS.Geometry;
using ESRI.ArcGIS.ADF.CATIDs;

namespace CutPolygonsTaskCS
{
  /// <summary>
  /// CutWithoutSelection implements IEditTask and IEditTaskName. 
  /// It determines which polygon features to cut from the target layer. 
  /// This task makes it easier to cut many features in the target layer.
  /// </summary>

  [Guid("2489a49b-090c-4bdb-9c9e-e96ea32d29e7")]
  [ClassInterface(ClassInterfaceType.None)]
  [ProgId("CutPolygonsTaskCS.CutWithoutSelection")]
  public class CutWithoutSelection : IEditTask, IEditTaskName
  {
    #region COM Registration Function(s)
    [ComRegisterFunction()]
    [ComVisible(false)]
    static void RegisterFunction(Type registerType)
    {
      // Required for ArcGIS Component Category Registrar support
      ArcGISCategoryRegistration(registerType);

      //
      // TODO: Add any COM registration code here
      //
    }

    [ComUnregisterFunction()]
    [ComVisible(false)] 
    
    static void UnregisterFunction(Type registerType)
    {
      // Required for ArcGIS Component Category Registrar support
      ArcGISCategoryUnregistration(registerType);

      //
      // TODO: Add any COM unregistration code here
      //
    }

    #region ArcGIS Component Category Registrar generated code
    /// <summary>
    /// Required method for ArcGIS Component Category registration -
    /// Do not modify the contents of this method with the code editor.
    /// </summary>
    private static void ArcGISCategoryRegistration(Type registerType)
    {
      string regKey = string.Format("HKEY_CLASSES_ROOT\\CLSID\\{{{0}}}", registerType.GUID);
      EditTasks.Register(regKey);

    }
    /// <summary>
    /// Required method for ArcGIS Component Category unregistration -
    /// Do not modify the contents of this method with the code editor.
    /// </summary>
    private static void ArcGISCategoryUnregistration(Type registerType)
    {
      string regKey = string.Format("HKEY_CLASSES_ROOT\\CLSID\\{{{0}}}", registerType.GUID);
      EditTasks.Unregister(regKey);

    }

    #endregion
    #endregion

    private IEditor m_editor;
    private IEditSketch m_editSketch;
    private IEditLayers m_editLayer;
    private IMxDocument m_mxDoc;

    #region IEditTask Members

    void IEditTask.Activate(IEditor Editor, IEditTask oldTask)
    {
      if (Editor == null)
        return;

      //Initialize class member variables.
      m_editor = Editor;
      m_editSketch = Editor as IEditSketch;
      m_editSketch.GeometryType = esriGeometryType.esriGeometryPolyline;
      m_editLayer = Editor as IEditLayers;
      m_mxDoc = Editor.Parent.Document as IMxDocument;

      //Wire editor events.
      ((IEditEvents_Event)m_editor).OnCurrentLayerChanged += 
        new IEditEvents_OnCurrentLayerChangedEventHandler(OnCurrentLayerChanged);
    }

    void OnCurrentLayerChanged()
    {
      if (m_editLayer == null)
        return;

      //Only enable the sketch tool if there is a polygon target layer.
      if (m_editLayer.CurrentLayer.FeatureClass.ShapeType != esriGeometryType.esriGeometryPolygon)
        m_editSketch.GeometryType = esriGeometryType.esriGeometryNull;
      else
      //Set the edit sketch geometry type to be esriGeometryPolyline.
        m_editSketch.GeometryType = esriGeometryType.esriGeometryPolyline;
    }

    void IEditTask.Deactivate()
    {
      //Stop listening for editor events.
      ((IEditEvents_Event)m_editor).OnCurrentLayerChanged -= OnCurrentLayerChanged;

      //Release object references.
      m_editor = null;
      m_editSketch = null;
      m_editLayer = null;
      m_mxDoc = null;
   }

    string IEditTask.Name
    {
      get 
      {
        return "Cut Polygons Without Selection_CSharp";
      }
    }

    void IEditTask.OnDeleteSketch()
    {
    }

    void IEditTask.OnFinishSketch()
    {
      if (m_editSketch == null)
        return;

      bool HasCutPolygons = false;

      //Change the cursor to be hourglass shape.
      System.Windows.Forms.Cursor.Current = Cursors.WaitCursor;

      try
      {
        //Get the geometry that performs the cut from the edit sketch.
        IGeometry cutGeometry = m_editSketch.Geometry;

        //The sketch geometry is simplified to deal with a multi-part sketch as well
        //as the case where the sketch loops back over itself.
        ITopologicalOperator2 topoOperator = cutGeometry as ITopologicalOperator2;
        topoOperator.IsKnownSimple_2 = false;
        topoOperator.Simplify();

        //Get ready to invalidate the features.
        IInvalidArea refreshArea = new InvalidAreaClass();
        refreshArea.Add(cutGeometry.Envelope);

        //Create the spatial filter to search for features in the target feature class.
        //The spatial relationship we care about is whether the interior of the line 
        //intesects the interior of the polygon.
        ISpatialFilter spatialFilter = new SpatialFilterClass();
        spatialFilter.Geometry = m_editSketch.Geometry;
        spatialFilter.SpatialRel = esriSpatialRelEnum.esriSpatialRelIntersects;

        //Find the polygon features that cross the sketch.
        IFeatureClass featureClass = m_editLayer.CurrentLayer.FeatureClass;
        IFeatureCursor featureCursor = featureClass.Search(spatialFilter, false);

        //Only do work if there are features that intersect the edit sketch.
        IFeature origFeature = featureCursor.NextFeature();
        if (origFeature != null)
        {
          //Check the first feature to see if it is ZAware and if it needs to make the
          //cut geometry ZAware.
          IZAware zAware = origFeature.Shape as IZAware;
          if (zAware.ZAware)
          {
            zAware = cutGeometry as IZAware;
            zAware.ZAware = true;
          }

          //The result selection set will be the new features created. This means 
          //we need to add the currently selected features to the refresh area so 
          //they are correctly invalidated.
          if (m_editor.SelectionCount > 0)
          {
            IEnumFeature selectedFeatures = m_editor.EditSelection;
            selectedFeatures.Reset();
            IFeature selectedFeature = selectedFeatures.Next();
            while (selectedFeature != null)
            {
              refreshArea.Add(selectedFeature);
              selectedFeature = selectedFeatures.Next();
            }
          }

          //Initialize the selection set for the new features.
          IFeatureSelection featureSelect = m_editLayer.CurrentLayer as IFeatureSelection;
          featureSelect.Clear();
          //Cache the new features created.
          ISelectionSet selectionSet = featureSelect.SelectionSet;

          //Start an edit operation so we can have undo/redo.
          m_editor.StartOperation();

          //Cycle through the features, cutting with the sketch.
          while (origFeature != null)
          {

            try
            {
              //Add the feature to the invalidate object.
              refreshArea.Add(origFeature);

              //Split the feature. Use the IFeatureEdit::Split method which ensures
              //the attributes are correctly dealt with.
              IFeatureEdit featureEdit = origFeature as IFeatureEdit;
              //Set to hold the new features that are created by the Split.            
              ISet newFeaturesSet = featureEdit.Split(cutGeometry);

              //New features have been created, loop through the set and flash 
              //each feature and get it's OID for the final selection set.
              if (newFeaturesSet != null)
              {
                newFeaturesSet.Reset();

                //A list to hold the selected features' OIDs.
                List<int> iOIDList = new List<int>();

                for (int featureCount = 0; featureCount < newFeaturesSet.Count; featureCount++)
                {
                  HasCutPolygons = true;
                  //Flash the new features.
                  IFeature newFeature = newFeaturesSet.Next() as IFeature;
                  FlashGeometry(newFeature.Shape, m_editor.Display);

                  //Add the feature to the new selection set
                  iOIDList.Add(newFeature.OID);
                }
                int[] iOIDArray = iOIDList.ToArray();

                selectionSet.AddList(newFeaturesSet.Count, ref iOIDArray[0]);
              }
            }
            catch (COMException)
            { 
            }
            finally
            {
              //Continue to work on the next feature if it fails to split the current one.
              origFeature = featureCursor.NextFeature();
            }
          }
          //If any polygons were cut, invalidate the display and stop the edit operation.
          if (HasCutPolygons)
          {
            //Clear the map's selection.
            m_mxDoc.FocusMap.ClearSelection();

            //Select the new features.
            featureSelect.SelectionSet = selectionSet;

            //Refresh the display.
            refreshArea.Display = m_editor.Display;
            refreshArea.Invalidate((short)esriScreenCache.esriAllScreenCaches);

            //Complete the edit operation.
            m_editor.StopOperation("Cut Polygons Without Selection");

            //As we changed the selection set, the map has no idea we did this.
            //Fire the event to inform others the selection has changed.
            featureSelect.SelectionChanged();
          }
          else
            m_editor.AbortOperation();
        }
      }
      catch (Exception e)
      {
        MessageBox.Show("Unable to perform the cut task.\n" + e.ToString());
        //In the event of an error, abort the operation.
        m_editor.AbortOperation();
      }
      finally
      {
        //Change the cursor shape to default.
        System.Windows.Forms.Cursor.Current = Cursors.Default;
      }
    }

    private void FlashGeometry(IGeometry geo, IDisplay display)
    {
      //Flash the input polygon geometry.
      display.StartDrawing(display.hDC, (short)esriScreenCache.esriNoScreenCache);

      //Time in milliseconds to wait.
      int interval = 150;
      switch (geo.GeometryType)
      {
        case esriGeometryType.esriGeometryPolygon:
          //Set the flash geometry's symbol.
          IRgbColor color = new RgbColorClass();
          color.Red = 255;

          ISimpleFillSymbol simpleFillSymbol = new SimpleFillSymbolClass();
          simpleFillSymbol.Color = color;

          ISymbol symbol = simpleFillSymbol as ISymbol;
          symbol.ROP2 = esriRasterOpCode.esriROPNotXOrPen;

          display.SetSymbol(symbol);
          display.DrawPolygon(geo);
          System.Threading.Thread.Sleep(interval);
          display.DrawPolygon(geo);

          break;
      }
      display.FinishDrawing();
    }

    #endregion

    #region IEditTaskName Members

    string IEditTaskName.UniqueName
    {
      //Allow users to uniquely identify the edit task by name.
      get
      {
        return "CutPolygonsWithoutSelectionTask";
      }
    }

    #endregion


  }
}
