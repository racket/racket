using System;
using System.Runtime.InteropServices;
using System.Reflection;
using System.Reflection.Emit;
using System.Diagnostics;
using System.Diagnostics.SymbolStore;
using System.Threading;
using System.Collections;
using System.Windows.Forms;
using Microsoft.Office.Interop;

#if STRONG
[assembly: System.Reflection.AssemblyVersion ("1.0.0.0")]
[assembly: System.Reflection.AssemblyKeyFile ("orgKey.snk")]
#endif

namespace SchemeBridge
{
  // When a .NET object is actually a COM object with an
  // interop wrapper, it turns out that when you marshal it
  // back out to COM it just unwraps the object rather than
  // creating a COM callable wrapper.  Thus the standard
  // .NET methods like GetType() or ToString() won't work.

  // The BridgeWrapper object, being a bona fide .NET object
  // will have those methods.  The Scheme interface knows about
  // BridgeWrappers specially and presents the illusion that
  // they aren't there.

  public class BridgeWrapper
  {
    public Object underlying_object;

    public static Hashtable WrappedObjectTable = new Hashtable();

    BridgeWrapper (Object _underlying_object)
    {
      this.underlying_object = _underlying_object;
      WrappedObjectTable.Add (_underlying_object, this);
    }

    ~BridgeWrapper()
    {
      // Console.WriteLine ("Removing wrapper for " + this.underlying_object);
      WrappedObjectTable.Remove (this.underlying_object);
    }

    public Object Unwrap ()
    {
      return this.underlying_object;
    }

    public static Object Wrap (Object _underlying_object)
    {
      if (_underlying_object == null) return null;

      try {
          if (_underlying_object.GetType().IsCOMObject == false) {
              Console.WriteLine ("Not wrapping object " + _underlying_object.ToString());
              return _underlying_object;
              }
          }
      catch (System.Exception e) {
          ReportError ("in BridgeWrapper.Wrap.", e);
          throw;
          }

          Console.WriteLine ("Wrapping object " + _underlying_object.ToString());
      Object result = WrappedObjectTable[_underlying_object];
      if (result == null)
          return new BridgeWrapper (_underlying_object);
      else
          return result;
    }

    private static void ReportError (String where, Exception e)
    {
      Console.WriteLine ("CLR error " + where);
      Console.WriteLine ("Message is:  " + e.Message);
      Console.WriteLine ("Stack trace follows");
      Console.WriteLine ("--------");
      Console.WriteLine (e.StackTrace);
      Console.WriteLine ("--------");
      if (e.InnerException != null) {
          Console.WriteLine ("Inner exception present.");
          Console.WriteLine (e.InnerException);
          Console.WriteLine ("");
          }
      Console.WriteLine ("");
      Console.Out.Flush();
    }

    private Type ExcelType (String name)
    {
      Console.WriteLine ("It appears to be a " + name);
      return System.Reflection.Assembly.LoadWithPartialName ("Microsoft.Office.Interop.Excel").
          GetType ("Microsoft.Office.Interop.Excel." + name, true, true);
    }

    public Type UnwrappedGetType()
    {
      // See Microsoft Knowledge Base Article - 320523
      // My only reaction is:  Oh my god!!!!
      try {
           return
                 (underlying_object as Microsoft.Office.Interop.Excel.AddIn) != null ? ExcelType ("AddIn")
               : (underlying_object as Microsoft.Office.Interop.Excel.AddIns) != null ? ExcelType ("AddIns")
               : (underlying_object as Microsoft.Office.Interop.Excel.AllowEditRange) != null ? ExcelType ("AllowEditRange")
               : (underlying_object as Microsoft.Office.Interop.Excel.AllowEditRanges) != null ? ExcelType ("AllowEditRanges")
               : (underlying_object as Microsoft.Office.Interop.Excel.AppEvents) != null ? ExcelType ("AppEvents")
               : (underlying_object as Microsoft.Office.Interop.Excel.Arc) != null ? ExcelType ("Arc")
               : (underlying_object as Microsoft.Office.Interop.Excel.Arcs) != null ? ExcelType ("Arcs")
               : (underlying_object as Microsoft.Office.Interop.Excel.Areas) != null ? ExcelType ("Areas")
               : (underlying_object as Microsoft.Office.Interop.Excel.AutoCorrect) != null ? ExcelType ("AutoCorrect")
               : (underlying_object as Microsoft.Office.Interop.Excel.AutoFilter) != null ? ExcelType ("AutoFilter")
               : (underlying_object as Microsoft.Office.Interop.Excel.AutoRecover) != null ? ExcelType ("AutoRecover")
               : (underlying_object as Microsoft.Office.Interop.Excel.Axes) != null ? ExcelType ("Axes")
               : (underlying_object as Microsoft.Office.Interop.Excel.Axis) != null ? ExcelType ("Axis")
               : (underlying_object as Microsoft.Office.Interop.Excel.AxisTitle) != null ? ExcelType ("AxisTitle")
               : (underlying_object as Microsoft.Office.Interop.Excel.Border) != null ? ExcelType ("Border")
               : (underlying_object as Microsoft.Office.Interop.Excel.Borders) != null ? ExcelType ("Borders")
               : (underlying_object as Microsoft.Office.Interop.Excel.Button) != null ? ExcelType ("Button")
               : (underlying_object as Microsoft.Office.Interop.Excel.Buttons) != null ? ExcelType ("Buttons")
               : (underlying_object as Microsoft.Office.Interop.Excel.CalculatedFields) != null ? ExcelType ("CalculatedFields")
               : (underlying_object as Microsoft.Office.Interop.Excel.CalculatedItems) != null ? ExcelType ("CalculatedItems")
               : (underlying_object as Microsoft.Office.Interop.Excel.CalculatedMember) != null ? ExcelType ("CalculatedMember")
               : (underlying_object as Microsoft.Office.Interop.Excel.CalculatedMembers) != null ? ExcelType ("CalculatedMembers")
               : (underlying_object as Microsoft.Office.Interop.Excel.CellFormat) != null ? ExcelType ("CellFormat")
               : (underlying_object as Microsoft.Office.Interop.Excel.Characters) != null ? ExcelType ("Characters")
               : (underlying_object as Microsoft.Office.Interop.Excel.ChartArea) != null ? ExcelType ("ChartArea")
               : (underlying_object as Microsoft.Office.Interop.Excel.ChartColorFormat) != null ? ExcelType ("ChartColorFormat")
               : (underlying_object as Microsoft.Office.Interop.Excel.ChartEvents) != null ? ExcelType ("ChartEvents")
               : (underlying_object as Microsoft.Office.Interop.Excel.ChartFillFormat) != null ? ExcelType ("ChartFillFormat")
               : (underlying_object as Microsoft.Office.Interop.Excel.ChartGroup) != null ? ExcelType ("ChartGroup")
               : (underlying_object as Microsoft.Office.Interop.Excel.ChartGroups) != null ? ExcelType ("ChartGroups")
               : (underlying_object as Microsoft.Office.Interop.Excel.ChartObject) != null ? ExcelType ("ChartObject")
               : (underlying_object as Microsoft.Office.Interop.Excel.ChartObjects) != null ? ExcelType ("ChartObjects")
               : (underlying_object as Microsoft.Office.Interop.Excel.ChartTitle) != null ? ExcelType ("ChartTitle")
               : (underlying_object as Microsoft.Office.Interop.Excel.Charts) != null ? ExcelType ("Charts")
               : (underlying_object as Microsoft.Office.Interop.Excel.CheckBox) != null ? ExcelType ("CheckBox")
               : (underlying_object as Microsoft.Office.Interop.Excel.CheckBoxes) != null ? ExcelType ("CheckBoxes")
               : (underlying_object as Microsoft.Office.Interop.Excel.Comment) != null ? ExcelType ("Comment")
               : (underlying_object as Microsoft.Office.Interop.Excel.Comment) != null ? ExcelType ("Comment")
               : (underlying_object as Microsoft.Office.Interop.Excel.Comments) != null ? ExcelType ("Comments")
               : (underlying_object as Microsoft.Office.Interop.Excel.Comments) != null ? ExcelType ("Comments")
               : (underlying_object as Microsoft.Office.Interop.Excel.ConnectorFormat) != null ? ExcelType ("ConnectorFormat")
               : (underlying_object as Microsoft.Office.Interop.Excel.ControlFormat) != null ? ExcelType ("ControlFormat")
               : (underlying_object as Microsoft.Office.Interop.Excel.Corners) != null ? ExcelType ("Corners")
               : (underlying_object as Microsoft.Office.Interop.Excel.CustomProperties) != null ? ExcelType ("CustomProperties")
               : (underlying_object as Microsoft.Office.Interop.Excel.CustomProperty) != null ? ExcelType ("CustomProperty")
               : (underlying_object as Microsoft.Office.Interop.Excel.CustomView) != null ? ExcelType ("CustomView")
               : (underlying_object as Microsoft.Office.Interop.Excel.CustomViews) != null ? ExcelType ("CustomViews")
               : (underlying_object as Microsoft.Office.Interop.Excel.DataLabel) != null ? ExcelType ("DataLabel")
               : (underlying_object as Microsoft.Office.Interop.Excel.DataLabels) != null ? ExcelType ("DataLabels")
               : (underlying_object as Microsoft.Office.Interop.Excel.DataTable) != null ? ExcelType ("DataTable")
               : (underlying_object as Microsoft.Office.Interop.Excel.Diagram) != null ? ExcelType ("Diagram")
               : (underlying_object as Microsoft.Office.Interop.Excel.Dialog) != null ? ExcelType ("Dialog")
               : (underlying_object as Microsoft.Office.Interop.Excel.DialogFrame) != null ? ExcelType ("DialogFrame")
               : (underlying_object as Microsoft.Office.Interop.Excel.DialogSheet) != null ? ExcelType ("DialogSheet")
               : (underlying_object as Microsoft.Office.Interop.Excel.DialogSheets) != null ? ExcelType ("DialogSheets")
               : (underlying_object as Microsoft.Office.Interop.Excel.Dialogs) != null ? ExcelType ("Dialogs")
               : (underlying_object as Microsoft.Office.Interop.Excel.DisplayUnitLabel) != null ? ExcelType ("DisplayUnitLabel")
               : (underlying_object as Microsoft.Office.Interop.Excel.DocEvents) != null ? ExcelType ("DocEvents")
               : (underlying_object as Microsoft.Office.Interop.Excel.DownBars) != null ? ExcelType ("DownBars")
               : (underlying_object as Microsoft.Office.Interop.Excel.Drawing) != null ? ExcelType ("Drawing")
               : (underlying_object as Microsoft.Office.Interop.Excel.DrawingObjects) != null ? ExcelType ("DrawingObjects")
               : (underlying_object as Microsoft.Office.Interop.Excel.Drawings) != null ? ExcelType ("Drawings")
               : (underlying_object as Microsoft.Office.Interop.Excel.DropDown) != null ? ExcelType ("DropDown")
               : (underlying_object as Microsoft.Office.Interop.Excel.DropDowns) != null ? ExcelType ("DropDowns")
               : (underlying_object as Microsoft.Office.Interop.Excel.DropLines) != null ? ExcelType ("DropLines")
               : (underlying_object as Microsoft.Office.Interop.Excel.EditBox) != null ? ExcelType ("EditBox")
               : (underlying_object as Microsoft.Office.Interop.Excel.EditBoxes) != null ? ExcelType ("EditBoxes")
               : (underlying_object as Microsoft.Office.Interop.Excel.Error) != null ? ExcelType ("Error")
               : (underlying_object as Microsoft.Office.Interop.Excel.ErrorBars) != null ? ExcelType ("ErrorBars")
               : (underlying_object as Microsoft.Office.Interop.Excel.ErrorCheckingOptions) != null ? ExcelType ("ErrorCheckingOptions")
               : (underlying_object as Microsoft.Office.Interop.Excel.Errors) != null ? ExcelType ("Errors")
               : (underlying_object as Microsoft.Office.Interop.Excel.Filter) != null ? ExcelType ("Filter")
               : (underlying_object as Microsoft.Office.Interop.Excel.Filters) != null ? ExcelType ("Filters")
               : (underlying_object as Microsoft.Office.Interop.Excel.Floor) != null ? ExcelType ("Floor")
               : (underlying_object as Microsoft.Office.Interop.Excel.Font) != null ? ExcelType ("Font")
               : (underlying_object as Microsoft.Office.Interop.Excel.FormatCondition) != null ? ExcelType ("FormatCondition")
               : (underlying_object as Microsoft.Office.Interop.Excel.FormatConditions) != null ? ExcelType ("FormatConditions")
               : (underlying_object as Microsoft.Office.Interop.Excel.FreeformBuilder) != null ? ExcelType ("FreeformBuilder")
               : (underlying_object as Microsoft.Office.Interop.Excel.Graphic) != null ? ExcelType ("Graphic")
               : (underlying_object as Microsoft.Office.Interop.Excel.Gridlines) != null ? ExcelType ("Gridlines")
               : (underlying_object as Microsoft.Office.Interop.Excel.GroupBox) != null ? ExcelType ("GroupBox")
               : (underlying_object as Microsoft.Office.Interop.Excel.GroupBoxes) != null ? ExcelType ("GroupBoxes")
               : (underlying_object as Microsoft.Office.Interop.Excel.GroupObject) != null ? ExcelType ("GroupObject")
               : (underlying_object as Microsoft.Office.Interop.Excel.GroupObjects) != null ? ExcelType ("GroupObjects")
               : (underlying_object as Microsoft.Office.Interop.Excel.GroupShapes) != null ? ExcelType ("GroupShapes")
               : (underlying_object as Microsoft.Office.Interop.Excel.HPageBreak) != null ? ExcelType ("HPageBreak")
               : (underlying_object as Microsoft.Office.Interop.Excel.HPageBreaks) != null ? ExcelType ("HPageBreaks")
               : (underlying_object as Microsoft.Office.Interop.Excel.HiLoLines) != null ? ExcelType ("HiLoLines")
               : (underlying_object as Microsoft.Office.Interop.Excel.Hyperlink) != null ? ExcelType ("Hyperlink")
               : (underlying_object as Microsoft.Office.Interop.Excel.Hyperlinks) != null ? ExcelType ("Hyperlinks")
               : (underlying_object as Microsoft.Office.Interop.Excel.Interior) != null ? ExcelType ("Interior")
               : (underlying_object as Microsoft.Office.Interop.Excel.Label) != null ? ExcelType ("Label")
               : (underlying_object as Microsoft.Office.Interop.Excel.Labels) != null ? ExcelType ("Labels")
               : (underlying_object as Microsoft.Office.Interop.Excel.LeaderLines) != null ? ExcelType ("LeaderLines")
               : (underlying_object as Microsoft.Office.Interop.Excel.Legend) != null ? ExcelType ("Legend")
               : (underlying_object as Microsoft.Office.Interop.Excel.LegendEntries) != null ? ExcelType ("LegendEntries")
               : (underlying_object as Microsoft.Office.Interop.Excel.LegendEntry) != null ? ExcelType ("LegendEntry")
               : (underlying_object as Microsoft.Office.Interop.Excel.LegendKey) != null ? ExcelType ("LegendKey")
               : (underlying_object as Microsoft.Office.Interop.Excel.Line) != null ? ExcelType ("Line")
               : (underlying_object as Microsoft.Office.Interop.Excel.Lines) != null ? ExcelType ("Lines")
               : (underlying_object as Microsoft.Office.Interop.Excel.LinkFormat) != null ? ExcelType ("LinkFormat")
               : (underlying_object as Microsoft.Office.Interop.Excel.ListBox) != null ? ExcelType ("ListBox")
               : (underlying_object as Microsoft.Office.Interop.Excel.ListBoxes) != null ? ExcelType ("ListBoxes")
               : (underlying_object as Microsoft.Office.Interop.Excel.Mailer) != null ? ExcelType ("Mailer")
               : (underlying_object as Microsoft.Office.Interop.Excel.Menu) != null ? ExcelType ("Menu")
               : (underlying_object as Microsoft.Office.Interop.Excel.MenuBar) != null ? ExcelType ("MenuBar")
               : (underlying_object as Microsoft.Office.Interop.Excel.MenuBars) != null ? ExcelType ("MenuBars")
               : (underlying_object as Microsoft.Office.Interop.Excel.MenuItem) != null ? ExcelType ("MenuItem")
               : (underlying_object as Microsoft.Office.Interop.Excel.MenuItems) != null ? ExcelType ("MenuItems")
               : (underlying_object as Microsoft.Office.Interop.Excel.Menus) != null ? ExcelType ("Menus")
               : (underlying_object as Microsoft.Office.Interop.Excel.Module) != null ? ExcelType ("Module")
               : (underlying_object as Microsoft.Office.Interop.Excel.Modules) != null ? ExcelType ("Modules")
               : (underlying_object as Microsoft.Office.Interop.Excel.Name) != null ? ExcelType ("Name")
               : (underlying_object as Microsoft.Office.Interop.Excel.Names) != null ? ExcelType ("Names")
               : (underlying_object as Microsoft.Office.Interop.Excel.ODBCError) != null ? ExcelType ("ODBCError")
               : (underlying_object as Microsoft.Office.Interop.Excel.ODBCErrors) != null ? ExcelType ("ODBCErrors")
               : (underlying_object as Microsoft.Office.Interop.Excel.OLEDBError) != null ? ExcelType ("OLEDBError")
               : (underlying_object as Microsoft.Office.Interop.Excel.OLEDBErrors) != null ? ExcelType ("OLEDBErrors")
               : (underlying_object as Microsoft.Office.Interop.Excel.OLEFormat) != null ? ExcelType ("OLEFormat")
               : (underlying_object as Microsoft.Office.Interop.Excel.OLEObjectEvents) != null ? ExcelType ("OLEObjectEvents")
               : (underlying_object as Microsoft.Office.Interop.Excel.OLEObjects) != null ? ExcelType ("OLEObjects")
               : (underlying_object as Microsoft.Office.Interop.Excel.OptionButton) != null ? ExcelType ("OptionButton")
               : (underlying_object as Microsoft.Office.Interop.Excel.OptionButtons) != null ? ExcelType ("OptionButtons")
               : (underlying_object as Microsoft.Office.Interop.Excel.Outline) != null ? ExcelType ("Outline")
               : (underlying_object as Microsoft.Office.Interop.Excel.Oval) != null ? ExcelType ("Oval")
               : (underlying_object as Microsoft.Office.Interop.Excel.Ovals) != null ? ExcelType ("Ovals")
               : (underlying_object as Microsoft.Office.Interop.Excel.PageSetup) != null ? ExcelType ("PageSetup")
               : (underlying_object as Microsoft.Office.Interop.Excel.Pane) != null ? ExcelType ("Pane")
               : (underlying_object as Microsoft.Office.Interop.Excel.Panes) != null ? ExcelType ("Panes")
               : (underlying_object as Microsoft.Office.Interop.Excel.Parameter) != null ? ExcelType ("Parameter")
               : (underlying_object as Microsoft.Office.Interop.Excel.Parameters) != null ? ExcelType ("Parameters")
               : (underlying_object as Microsoft.Office.Interop.Excel.Phonetic) != null ? ExcelType ("Phonetic")
               : (underlying_object as Microsoft.Office.Interop.Excel.Phonetics) != null ? ExcelType ("Phonetics")
               : (underlying_object as Microsoft.Office.Interop.Excel.Picture) != null ? ExcelType ("Picture")
               : (underlying_object as Microsoft.Office.Interop.Excel.Pictures) != null ? ExcelType ("Pictures")
               : (underlying_object as Microsoft.Office.Interop.Excel.PivotCache) != null ? ExcelType ("PivotCache")
               : (underlying_object as Microsoft.Office.Interop.Excel.PivotCaches) != null ? ExcelType ("PivotCaches")
               : (underlying_object as Microsoft.Office.Interop.Excel.PivotCell) != null ? ExcelType ("PivotCell")
               : (underlying_object as Microsoft.Office.Interop.Excel.PivotField) != null ? ExcelType ("PivotField")
               : (underlying_object as Microsoft.Office.Interop.Excel.PivotFields) != null ? ExcelType ("PivotFields")
               : (underlying_object as Microsoft.Office.Interop.Excel.PivotFormula) != null ? ExcelType ("PivotFormula")
               : (underlying_object as Microsoft.Office.Interop.Excel.PivotFormulas) != null ? ExcelType ("PivotFormulas")
               : (underlying_object as Microsoft.Office.Interop.Excel.PivotItem) != null ? ExcelType ("PivotItem")
               : (underlying_object as Microsoft.Office.Interop.Excel.PivotItemList) != null ? ExcelType ("PivotItemList")
               : (underlying_object as Microsoft.Office.Interop.Excel.PivotItems) != null ? ExcelType ("PivotItems")
               : (underlying_object as Microsoft.Office.Interop.Excel.PivotLayout) != null ? ExcelType ("PivotLayout")
               : (underlying_object as Microsoft.Office.Interop.Excel.PivotTable) != null ? ExcelType ("PivotTable")
               : (underlying_object as Microsoft.Office.Interop.Excel.PivotTables) != null ? ExcelType ("PivotTables")
               : (underlying_object as Microsoft.Office.Interop.Excel.PlotArea) != null ? ExcelType ("PlotArea")
               : (underlying_object as Microsoft.Office.Interop.Excel.Point) != null ? ExcelType ("Point")
               : (underlying_object as Microsoft.Office.Interop.Excel.Points) != null ? ExcelType ("Points")
               : (underlying_object as Microsoft.Office.Interop.Excel.Protection) != null ? ExcelType ("Protection")
               : (underlying_object as Microsoft.Office.Interop.Excel.PublishObjects) != null ? ExcelType ("PublishObjects")
               : (underlying_object as Microsoft.Office.Interop.Excel.QueryTables) != null ? ExcelType ("QueryTables")
               : (underlying_object as Microsoft.Office.Interop.Excel.RTD) != null ? ExcelType ("RTD")
               : (underlying_object as Microsoft.Office.Interop.Excel.Range) != null ? ExcelType ("Range")
               : (underlying_object as Microsoft.Office.Interop.Excel.Range) != null ? ExcelType ("Range")
               : (underlying_object as Microsoft.Office.Interop.Excel.RecentFile) != null ? ExcelType ("RecentFile")
               : (underlying_object as Microsoft.Office.Interop.Excel.RecentFiles) != null ? ExcelType ("RecentFiles")
               : (underlying_object as Microsoft.Office.Interop.Excel.Rectangle) != null ? ExcelType ("Rectangle")
               : (underlying_object as Microsoft.Office.Interop.Excel.Rectangles) != null ? ExcelType ("Rectangles")
               : (underlying_object as Microsoft.Office.Interop.Excel.RefreshEvents) != null ? ExcelType ("RefreshEvents")
               : (underlying_object as Microsoft.Office.Interop.Excel.RoutingSlip) != null ? ExcelType ("RoutingSlip")
               : (underlying_object as Microsoft.Office.Interop.Excel.Scenario) != null ? ExcelType ("Scenario")
               : (underlying_object as Microsoft.Office.Interop.Excel.Scenarios) != null ? ExcelType ("Scenarios")
               : (underlying_object as Microsoft.Office.Interop.Excel.ScrollBar) != null ? ExcelType ("ScrollBar")
               : (underlying_object as Microsoft.Office.Interop.Excel.ScrollBars) != null ? ExcelType ("ScrollBars")
               : (underlying_object as Microsoft.Office.Interop.Excel.Series) != null ? ExcelType ("Series")
               : (underlying_object as Microsoft.Office.Interop.Excel.SeriesCollection) != null ? ExcelType ("SeriesCollection")
               : (underlying_object as Microsoft.Office.Interop.Excel.SeriesLines) != null ? ExcelType ("SeriesLines")
               : (underlying_object as Microsoft.Office.Interop.Excel.Shape) != null ? ExcelType ("Shape")
               : (underlying_object as Microsoft.Office.Interop.Excel.ShapeRange) != null ? ExcelType ("ShapeRange")
               : (underlying_object as Microsoft.Office.Interop.Excel.Shapes) != null ? ExcelType ("Shapes")
               : (underlying_object as Microsoft.Office.Interop.Excel.Sheets) != null ? ExcelType ("Sheets")
               : (underlying_object as Microsoft.Office.Interop.Excel.SmartTag) != null ? ExcelType ("SmartTag")
               : (underlying_object as Microsoft.Office.Interop.Excel.SmartTagAction) != null ? ExcelType ("SmartTagAction")
               : (underlying_object as Microsoft.Office.Interop.Excel.SmartTagActions) != null ? ExcelType ("SmartTagActions")
               : (underlying_object as Microsoft.Office.Interop.Excel.SmartTagOptions) != null ? ExcelType ("SmartTagOptions")
               : (underlying_object as Microsoft.Office.Interop.Excel.SmartTagRecognizer) != null ? ExcelType ("SmartTagRecognizer")
               : (underlying_object as Microsoft.Office.Interop.Excel.SmartTagRecognizers) != null ? ExcelType ("SmartTagRecognizers")
               : (underlying_object as Microsoft.Office.Interop.Excel.SmartTags) != null ? ExcelType ("SmartTags")
               : (underlying_object as Microsoft.Office.Interop.Excel.SoundNote) != null ? ExcelType ("SoundNote")
               : (underlying_object as Microsoft.Office.Interop.Excel.Speech) != null ? ExcelType ("Speech")
               : (underlying_object as Microsoft.Office.Interop.Excel.SpellingOptions) != null ? ExcelType ("SpellingOptions")
               : (underlying_object as Microsoft.Office.Interop.Excel.Spinner) != null ? ExcelType ("Spinner")
               : (underlying_object as Microsoft.Office.Interop.Excel.Spinners) != null ? ExcelType ("Spinners")
               : (underlying_object as Microsoft.Office.Interop.Excel.Style) != null ? ExcelType ("Style")
               : (underlying_object as Microsoft.Office.Interop.Excel.Styles) != null ? ExcelType ("Styles")
               : (underlying_object as Microsoft.Office.Interop.Excel.Tab) != null ? ExcelType ("Tab")
               : (underlying_object as Microsoft.Office.Interop.Excel.TextBox) != null ? ExcelType ("TextBox")
               : (underlying_object as Microsoft.Office.Interop.Excel.TextBoxes) != null ? ExcelType ("TextBoxes")
               : (underlying_object as Microsoft.Office.Interop.Excel.TextFrame) != null ? ExcelType ("TextFrame")
               : (underlying_object as Microsoft.Office.Interop.Excel.TickLabels) != null ? ExcelType ("TickLabels")
               : (underlying_object as Microsoft.Office.Interop.Excel.Toolbar) != null ? ExcelType ("Toolbar")
               : (underlying_object as Microsoft.Office.Interop.Excel.ToolbarButton) != null ? ExcelType ("ToolbarButton")
               : (underlying_object as Microsoft.Office.Interop.Excel.ToolbarButtons) != null ? ExcelType ("ToolbarButtons")
               : (underlying_object as Microsoft.Office.Interop.Excel.Toolbars) != null ? ExcelType ("Toolbars")
               : (underlying_object as Microsoft.Office.Interop.Excel.Trendline) != null ? ExcelType ("Trendline")
               : (underlying_object as Microsoft.Office.Interop.Excel.Trendlines) != null ? ExcelType ("Trendlines")
               : (underlying_object as Microsoft.Office.Interop.Excel.UpBars) != null ? ExcelType ("UpBars")
               : (underlying_object as Microsoft.Office.Interop.Excel.UsedObjects) != null ? ExcelType ("UsedObjects")
               : (underlying_object as Microsoft.Office.Interop.Excel.UserAccess) != null ? ExcelType ("UserAccess")
               : (underlying_object as Microsoft.Office.Interop.Excel.UserAccessList) != null ? ExcelType ("UserAccessList")
               : (underlying_object as Microsoft.Office.Interop.Excel.VPageBreak) != null ? ExcelType ("VPageBreak")
               : (underlying_object as Microsoft.Office.Interop.Excel.VPageBreaks) != null ? ExcelType ("VPageBreaks")
               : (underlying_object as Microsoft.Office.Interop.Excel.Validation) != null ? ExcelType ("Validation")
               : (underlying_object as Microsoft.Office.Interop.Excel.Walls) != null ? ExcelType ("Walls")
               : (underlying_object as Microsoft.Office.Interop.Excel.Watch) != null ? ExcelType ("Watch")
               : (underlying_object as Microsoft.Office.Interop.Excel.Watches) != null ? ExcelType ("Watches")
               : (underlying_object as Microsoft.Office.Interop.Excel.Window) != null ? ExcelType ("Window")
               : (underlying_object as Microsoft.Office.Interop.Excel.Windows) != null ? ExcelType ("Windows")
               : (underlying_object as Microsoft.Office.Interop.Excel.WorkbookEvents) != null ? ExcelType ("WorkbookEvents")
               : (underlying_object as Microsoft.Office.Interop.Excel.Workbooks) != null ? ExcelType ("Workbooks")
               : (underlying_object as Microsoft.Office.Interop.Excel._Worksheet) != null ? ExcelType ("_Worksheet")
               : (underlying_object as Microsoft.Office.Interop.Excel.Worksheet) != null ? ExcelType ("Worksheet")
               : (underlying_object as Microsoft.Office.Interop.Excel.WorksheetFunction) != null ? ExcelType ("WorksheetFunction")
               : (underlying_object as Microsoft.Office.Interop.Excel.Worksheets) != null ? ExcelType ("Worksheets")
               : (underlying_object as Microsoft.Office.Interop.Excel._OLEObject) != null ? ExcelType ("_OLEObject")
               : (underlying_object as Microsoft.Office.Interop.Excel._QueryTable) != null ? ExcelType ("_QueryTable")
               : underlying_object.GetType();
           }
       catch (System.Exception e) {
           ReportError ("in GetType().", e);
           throw;
           }
    }

    public String UnwrappedToString()
    {
      try {
          return underlying_object.ToString();
          }
      catch (System.Exception e) {
          ReportError ("in ToString().", e);
          throw;
          }
    }

    public Object UnwrappedGetPropertyZero (String property)
    {
      FieldInfo [] fields = underlying_object.GetType().GetFields();
      FieldInfo field_candidate = null;
      foreach (FieldInfo scan in fields)
          if (String.Compare (scan.Name, property, true) == 0)
              if (field_candidate == null)
                  field_candidate = scan;
              else
                  throw new AmbiguousMatchException ("Property " + property + " has multiple definitions.");
      if (field_candidate != null)
          try {
              return BridgeWrapper.Wrap (field_candidate.GetValue (underlying_object));
              }
          catch (Exception e) {
              Console.Write ("CLR error during field reference.\n");
              Console.Write ("Message is:  " + e.Message + "\n");
              Console.Write ("Stack trace follows\n--------\n");
              Console.Write (e.StackTrace + "\n--------\n\n");
              if (e.InnerException != null) {
                  Console.Write ("Inner exception present.\n\n" + e.InnerException);
                  Console.Write ("\n\n");
                  }

              throw e;
              };

      PropertyInfo [] properties = underlying_object.GetType().GetProperties();
      PropertyInfo property_candidate = null;
      foreach (PropertyInfo scan in properties)
          if (String.Compare (scan.Name, property, true) == 0)
              if (property_candidate == null)
                  property_candidate = scan;
              else
                  throw new AmbiguousMatchException ("Property " + property + " has multiple definitions.");
      if (property_candidate != null)
          try {
              return BridgeWrapper.Wrap (property_candidate.GetValue (underlying_object, null));
              }
          catch (Exception e) {
              Console.Write ("CLR error during property reference.\n");
              Console.Write ("Message is:  " + e.Message + "\n");
              Console.Write ("Stack trace follows\n--------\n");
              Console.Write (e.StackTrace + "\n--------\n\n");
              if (e.InnerException != null) {
                  Console.Write ("Inner exception present.\n\n" + e.InnerException);
                  Console.Write ("\n\n");
                  }

              throw e;
              };

      throw new Exception ("No such property: " + property);
    }

    public System.Text.StringBuilder  _consalot ()
    {
      return new System.Text.StringBuilder (1000);
    }

    public void _UnwrappedInvokeZero (Object [] methods)
    {
      Console.WriteLine ("Entering _UnwrappedInvokeZero");
      Console.Out.Flush();
      int length = methods.Length;
      Console.WriteLine ("There are " + length.ToString() + " methods.");
      Console.Out.Flush();
      int j = 0x5a5a5a5a;
      int i;
      int k = 0x0f0f0f0f;
      for (i = 0; i < length; i++) {
          _consalot();
          Console.WriteLine ("i = " +  i.ToString());
          Console.Out.Flush();
          try {
              Console.WriteLine ("Entering try form in _UnwrappedInvokeZero." + i.ToString());
              Console.Out.Flush();
              Object mi = methods[i];
              Console.WriteLine ("Got value in _UnwrappedInvokeZero." + i.ToString());
              Console.Out.Flush();
              Console.WriteLine (j.ToString() + " " + i.ToString() + " " + k.ToString() + " " + ((mi == null)? "null" : mi.ToString()));
              Console.Out.Flush();
              }
          catch (Exception) {
              Console.WriteLine ("Catcher 0");
              Console.Out.Flush();
              Console.WriteLine ("Skipping index " + i.ToString());
              Console.Out.Flush();
              }
          finally {
              Console.WriteLine ("Exiting try form in _UnwrappedInvokeZero." + i.ToString());
              Console.Out.Flush();
              }
          Console.WriteLine ("Going around the loop again, i = " + i.ToString());
          Console.Out.Flush();
          }
      Console.WriteLine ("Returning from _UnwrappedInvokeZero");
      Console.Out.Flush();
    }

    public Object UnwrappedInvokeZero (String method)
    {
      try {
          Console.WriteLine ("Underlying object is " + underlying_object);
          Console.Out.Flush();

          MethodInfo candidate = null;
          Type type = underlying_object.GetType();
          Console.WriteLine ("Underlying object type is " + type.ToString());
          Console.Out.Flush();
          Object [] methods = underlying_object.GetType().GetMethods();
          Console.WriteLine ("Underlying object methods are " + methods.ToString());
          Console.Out.Flush();

          _UnwrappedInvokeZero (methods);


//           foreach (Object scan in underlying_object.GetType().GetMethods())
//               Console.WriteLine (scan);

          foreach (MethodInfo scan in underlying_object.GetType().GetMethods())
              if (String.Compare (scan.Name, method, true) == 0
                  && (scan.GetParameters() == null
                      || scan.GetParameters().Length == 2))
                  if (candidate == null)
                      candidate = scan;
                  else
                      throw new AmbiguousMatchException ("Method " + method + " has multiple definitions.");

          // if (candidate == null)
          //    throw new Exception ("Method " + method + " not found.");

          Object [] arglist = new Object[2];

          Console.WriteLine ("WTF?");
          Console.Out.Flush();
          return BridgeWrapper.Wrap (candidate.Invoke (underlying_object, arglist));
          }
      catch (Exception e) {
          Console.WriteLine ("Catcher 2");
          Console.Out.Flush();
          Console.WriteLine ("CLR error during method invokation.");
          Console.Out.Flush();
          Console.WriteLine ("Message is:  " + e.Message);
          Console.Out.Flush();
          Console.WriteLine ("Stack trace follows");
          Console.Out.Flush();
          Console.WriteLine ("--------");
          Console.Out.Flush();
          Console.WriteLine (e.StackTrace);
          Console.Out.Flush();
          Console.WriteLine ("--------");
          Console.Out.Flush();
          if (e.InnerException != null) {
              Console.WriteLine ("Inner exception present.\n" + e.InnerException);
              Console.Out.Flush();
              Console.WriteLine ("");
              Console.Out.Flush();
              Console.WriteLine ("");
              }

          throw e;
          }
    }

    public Object UnwrappedInvokeOne (String method, Object argument)
    {
      MethodInfo candidate = null;
      foreach (MethodInfo scan in underlying_object.GetType().GetMethods())
          if (String.Compare (scan.Name, method, true) == 0
              && scan.GetParameters().Length == 1
              && argument.GetType().IsSubclassOf (scan.GetParameters()[0].ParameterType))
              if (candidate == null)
                  candidate = scan;
              else
                  throw new AmbiguousMatchException ("Method " + method + " has multiple definitions.");
      try {
          Object [] args = new Object [1];
          args [0] = argument;
          return BridgeWrapper.Wrap (candidate.Invoke (underlying_object, args));
          }
      catch (Exception e) {
          Console.Write ("CLR error during method invokation.\n");
          Console.Write ("Message is:  " + e.Message + "\n");
          Console.Write ("Stack trace follows\n--------\n");
          Console.Write (e.StackTrace + "\n--------\n\n");
          if (e.InnerException != null) {
              Console.Write ("Inner exception present.\n\n" + e.InnerException);
              Console.Write ("\n\n");
              }

          throw e;
          }
    }

    public Object UnwrappedInvokeTwo (String method, Object arg0, Object arg1)
    {
      MethodInfo candidate = null;
      foreach (MethodInfo scan in underlying_object.GetType().GetMethods())
          if (String.Compare (scan.Name, method, true) == 0
              && scan.GetParameters().Length == 2
              && arg0.GetType().IsSubclassOf (scan.GetParameters()[0].ParameterType)
              && arg1.GetType().IsSubclassOf (scan.GetParameters()[1].ParameterType))
              if (candidate == null)
                  candidate = scan;
              else
                  throw new AmbiguousMatchException ("Method " + method + " has multiple definitions.");
      try {
          Object [] args = new Object [2];
          args [0] = arg0;
          args [1] = arg1;
          return BridgeWrapper.Wrap (candidate.Invoke (underlying_object, args));
          }
      catch (Exception e) {
          Console.Write ("CLR error during method invokation.\n");
          Console.Write ("Message is:  " + e.Message + "\n");
          Console.Write ("Stack trace follows\n--------\n");
          Console.Write (e.StackTrace + "\n--------\n\n");
          if (e.InnerException != null) {
              Console.Write ("Inner exception present.\n\n" + e.InnerException);
              Console.Write ("\n\n");
              }

          throw e;
          }
    }

    public Object UnwrappedInvokeThree (String method, Object arg0, Object arg1, Object arg2)
    {
      MethodInfo candidate = null;
      foreach (MethodInfo scan in underlying_object.GetType().GetMethods())
          if (String.Compare (scan.Name, method, true) == 0
              && scan.GetParameters().Length == 2
              && arg0.GetType().IsSubclassOf (scan.GetParameters()[0].ParameterType)
              && arg1.GetType().IsSubclassOf (scan.GetParameters()[1].ParameterType)
              && arg2.GetType().IsSubclassOf (scan.GetParameters()[2].ParameterType)
              )
              if (candidate == null)
                  candidate = scan;
              else
                  throw new AmbiguousMatchException ("Method " + method + " has multiple definitions.");
      try {
          Object [] args = new Object [3];
          args [0] = arg0;
          args [1] = arg1;
          args [2] = arg2;
          return BridgeWrapper.Wrap (candidate.Invoke (underlying_object, args));
          }
      catch (Exception e) {
          Console.Write ("CLR error during method invokation.\n");
          Console.Write ("Message is:  " + e.Message + "\n");
          Console.Write ("Stack trace follows\n--------\n");
          Console.Write (e.StackTrace + "\n--------\n\n");
          if (e.InnerException != null) {
              Console.Write ("Inner exception present.\n\n" + e.InnerException);
              Console.Write ("\n\n");
              }

          throw e;
          }
    }

    public Object UnwrappedInvokeFour (String method, Object arg0, Object arg1, Object arg2, Object arg3)
    {
      MethodInfo candidate = null;
      foreach (MethodInfo scan in underlying_object.GetType().GetMethods())
          if (String.Compare (scan.Name, method, true) == 0
              && scan.GetParameters().Length == 3
              && arg0.GetType().IsSubclassOf (scan.GetParameters()[0].ParameterType)
              && arg1.GetType().IsSubclassOf (scan.GetParameters()[1].ParameterType)
              && arg2.GetType().IsSubclassOf (scan.GetParameters()[2].ParameterType)
              && arg3.GetType().IsSubclassOf (scan.GetParameters()[3].ParameterType)
              )
              if (candidate == null)
                  candidate = scan;
              else
                  throw new AmbiguousMatchException ("Method " + method + " has multiple definitions.");
      try {
          Object [] args = new Object [3];
          args [0] = arg0;
          args [1] = arg1;
          args [2] = arg2;
          args [3] = arg3;
          return BridgeWrapper.Wrap (candidate.Invoke (underlying_object, args));
          }
      catch (Exception e) {
          Console.Write ("CLR error during method invokation.\n");
          Console.Write ("Message is:  " + e.Message + "\n");
          Console.Write ("Stack trace follows\n--------\n");
          Console.Write (e.StackTrace + "\n--------\n\n");
          if (e.InnerException != null) {
              Console.Write ("Inner exception present.\n\n" + e.InnerException);
              Console.Write ("\n\n");
              }

          throw e;
          }
    }

  }

  // funky way to get around array marshaling problem
  // in com interop.
  public class BridgeArray {

    public Object[] underlying_array;

    public void Allocate (int size)
    {
      this.underlying_array = new Object [size];
    }

    public void Aset (int index, Object thing)
    {
      this.underlying_array [index] = thing;
    }

    public void Aset (int index, BridgeWrapper thing)
    {
      this.underlying_array [index] = thing.underlying_object;
    }

    public void Aset (int index, String thing)
    {
      this.underlying_array [index] = thing;
    }

    public void Aset (int index, int thing)
    {
      this.underlying_array [index] = thing;
    }

    public int Length
    {
      get {
          return underlying_array.Length;
          }
    }

    public Object this[int index]
    {
      get {
          return underlying_array[index];
          }
    }

    public static implicit operator System.Object[] (BridgeArray source)
    {
      return source.underlying_array;
    }

    public System.Object[] ToArray ()
    {
      return this.underlying_array;
    }

  }


  // Outgoing delegate
  public delegate void Callback0Delegate (String thing);
  public delegate void Callback1Delegate (String thing, Object arg0);

  // Outgoing event interface that we will sink
  //[InterfaceType (ComInterfaceType.InterfaceIsIDispatch)]
  //[ Guid ("9376D78B-2B25-3DD3-9F2E-4369A1B99A01"),
  [ Guid ("DA064DCC-0881-11D3-B5CA-0060089002FE")]
  [InterfaceType (ComInterfaceType.InterfaceIsIDispatch)]
  // Where did I get that number from?
  public interface ISink
  {
    [DispId (-768)] void set_extension_table (uint address);
    [DispId (-769)] void set_myssink_table (int address);
    [DispId (-770)] void register_handler (int code, int address);
    [DispId (-771)] void unregister_handler (int code, int address);
    [DispId(42)]void Callback0(String thing);
    [DispId(43)]void Callback1(String thing, Object arg0);
  }

  public class Scheme
  {
    [DllImport ("libmzschxxxxxxx.dll")]
    [return: MarshalAs (UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof (MzSchemeObject))]
    public static extern MzSchemeObject scheme_make_integer_value (int x);

    [DllImport ("libmzschxxxxxxx.dll")]
    [return: MarshalAs (UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof (MzSchemeObject))]
    public static extern MzSchemeObject scheme_make_pair
    ([MarshalAs (UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof (MzSchemeObject))] MzSchemeObject car,
     [MarshalAs (UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof (MzSchemeObject))] MzSchemeObject cdr);

    [DllImport ("libmzschxxxxxxx.dll")]
    public static extern IntPtr scheme_make_true ();

    [DllImport ("libmzschxxxxxxx.dll")]
    public static extern IntPtr scheme_make_false ();

    [DllImport ("libmzschxxxxxxx.dll")]
    public static extern IntPtr scheme_make_string (String x);

    [DllImport ("libmzschxxxxxxx.dll")]
    public static extern void scheme_debug_print ([MarshalAs (UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof (MzSchemeObject))] MzSchemeObject obj);

    [DllImport ("libmzschxxxxxxx.dll")]
    public static extern IntPtr scheme_intern_symbol (String x);

    [DllImport ("libmzschxxxxxxx.dll")]
    [return: MarshalAs (UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof (MzSchemeObject))]
    public static extern MzSchemeObject scheme_car ([MarshalAs (UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof (MzSchemeObject))] MzSchemeObject pair);

    [DllImport ("libmzschxxxxxxx.dll")]
    [return: MarshalAs (UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof (MzSchemeObject))]
    public static extern MzSchemeObject scheme_apply
    ([MarshalAs (UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof (MzSchemeObject))] MzSchemeObject rator,
     int num_rands,
     IntPtr rands);
  }

  public enum Scheme_Type : ushort
  {
        Toplevel_Type,
        Local_Type,
        Local_Unbox_Type,
        Syntax_Type,
        Application_Type,
        Application2_Type,
        Application3_Type,
        Sequence_Type,
        Branch_Type,
        Unclosed_Procedure_Type,
        Let_Value_Type, /* 10 */
        Let_Void_Type,
        Letrec_Type,
        Let_One_Type,
        With_Cont_Mark_Type,

        _Values_Types_, /* All following types are values */

        /* intermediate compiled: */
        Compiled_Unclosed_Procedure_Type,
        Compiled_Let_Value_Type,
        Compiled_Let_Void_Type,
        Compiled_Syntax_Type,
        Compiled_Toplevel_Type,  /* 20 */
        Compiled_Quote_Syntax_Type,

        Quote_Compilation_Type, /* used while writing, only */

        /* Registered in prefix table: */
        Variable_Type,
        Module_Variable_Type, /* link replaces with scheme_variable_type */

        _Compiled_Values_Types_,

        /* procedure types */
        Prim_Type,
        Closed_Prim_Type,
        Closure_Type,
        Case_Closure_Type,
        Cont_Type, /* 30 */
        Escaping_Cont_Type,
        Proc_Struct_Type,

        /* structure types (overlaps with procs) */
        Structure_Type,

        /* basic types */
        Char_Type,
        Integer_Type,
        Bignum_Type,
        Rational_Type,
        Float_Type,
        Double_Type,
        Complex_Izi_Type, /* 40 */
        Complex_Type,
        String_Type,
        Symbol_Type,
        Null_Type,
        Pair_Type,
        Vector_Type,
        Inspector_Type,
        Input_Port_Type,
        Output_Port_Type,
        Eof_Type, /* 50 */
        True_Type,
        False_Type,
        Void_Type,
        Syntax_Compiler_Type,
        Macro_Type,
        Box_Type,
        Thread_Type,
        Stx_Offset_Type,
        Cont_Mark_Set_Type,
        Sema_Type, /* 60 */
        Hash_Table_Type,
        C_Pointer_Type,
        Weak_Box_Type,
        Struct_Type_Type,
        Module_Index_Type,
        Set_Macro_Type,
        Listener_Type,
        Namespace_Type,
        Config_Type,
        Stx_Type,  /* 70 */
        Will_Executor_Type,
        Custodian_Type,
        Random_State_Type,
        Regexp_Type,
        Bucket_Type,
        Bucket_Table_Type,
        Subprocess_Type,
        Compilation_Top_Type,
        Wrap_Chunk_Type,
        Eval_Waiting_Type, /* 80 */
        Tail_Call_Waiting_Type,
        Undefined_Type,
        Struct_Property_Type,
        Multiple_Values_Type,
        Placeholder_Type,
        Case_Lambda_Sequence_Type,
        Begin0_Sequence_Type,
        Rename_Table_Type,
        Module_Type,
        Svector_Type, /* 90 */
        Lazy_Macro_Type,
        Resolve_Prefix_Type,
        Security_Guard_Type,
        Indent_Type,
        Udp_Type,
        Udp_Waitable_Type,
        Id_Macro_Type,
        Waitable_Set_Type,
        Wrapped_Waitable_Type,
        Nack_Guard_Waitable_Type, /* 100 */
        Semaphore_Repost_Type,
        Channel_Type,
        Channel_Put_Type,
        Thread_Resume_Type,
        Thread_Suspend_Type,
        Thread_Dead_Type,
        Poll_Waitable_Type,
        Nack_Waitable_Type,

        Reserved1_Type
  }

  [StructLayout(LayoutKind.Explicit)]
  public struct ManifestMzSchemeObject
  {
    [FieldOffset (0)] public Scheme_Type type;
    [FieldOffset (2)] public ushort key;
  }

  [StructLayout(LayoutKind.Explicit)]
  public unsafe class MzSchemeObject : ICustomMarshaler
  {
    [FieldOffset (0)] public ManifestMzSchemeObject * handle;

    public MzSchemeObject ()
    {
    }

    public static implicit operator IntPtr (MzSchemeObject source)
    {
      return (IntPtr)(void *)(source.handle);
    }

    public void CleanUpManagedData (Object ManagedObject)
    {
      Console.WriteLine ("CleanUpManagedData " + ManagedObject.ToString());
      Console.Out.Flush();
    }

    public void CleanUpNativeData (IntPtr pNativeData)
    {
      // Console.WriteLine ("CleanUpNativeData (" + pNativeData.ToString() + ")");
      // Console.Out.Flush();
    }

    public int GetNativeDataSize ()
    {
      Console.WriteLine ("GetNativeDataSize()");
      Console.Out.Flush();
      return 4;
    }

    public IntPtr MarshalManagedToNative (Object ManagedObject)
    {

      if (ManagedObject is MzSchemeObject) {
          IntPtr result = (IntPtr) (MzSchemeObject) ManagedObject;
          // Console.WriteLine ("MarshalManagedToNative (" + ManagedObject.ToString() + ") => " + result.ToInt32().ToString());
          return result;
          }
      else if (ManagedObject is String) {
          IntPtr result = Scheme.scheme_intern_symbol ((String)ManagedObject);
          // Console.WriteLine ("MarshalManagedToNative (\"" + ManagedObject + "\") => " + result.ToInt32().ToString());
          return result;
          }
      else if (ManagedObject is int) {
          IntPtr result = new IntPtr ((Int32)(ManagedObject) * 2 + 1);
          // Console.WriteLine ("MarshalManagedToNative (" + ManagedObject.ToString() + ") => " + result.ToString());
          return result;
          }

      else if (ManagedObject is Boolean)
          return (ManagedObject.Equals (true)) ? Scheme.scheme_make_true() : Scheme.scheme_make_false();

      else  {
          Console.WriteLine ("MarshalManagedToNative (" + ManagedObject.ToString() + ") => 0");
          return (IntPtr)0;
          }

    }

    public Object MarshalNativeToManaged (IntPtr pNativeData)
    {
      // Console.WriteLine ("MarshalNativeToManaged (" + pNativeData.ToString() + ")");
      // Console.Out.Flush();
      if ((pNativeData.ToInt32() & 0x1) != 0)
          return (Object) (pNativeData.ToInt32() >> 1);

      else {
          ManifestMzSchemeObject * handle = (ManifestMzSchemeObject *)(void *) pNativeData;

          Scheme_Type type = handle->type;
          // Console.WriteLine ("Scheme_type is " + type);
          // Console.Out.Flush();

          switch (type) {
            case Scheme_Type.Void_Type:
              return DBNull.Value;

            case Scheme_Type.False_Type:
              return false;

            case Scheme_Type.Null_Type:
              return null;

            case Scheme_Type.String_Type:
              Console.WriteLine (pNativeData.ToInt32() + " " + ((IntPtr)(pNativeData.ToInt32() + 4)));
              Console.Out.Flush();
              return new System.Text.StringBuilder (Marshal.PtrToStringAnsi ((IntPtr)(Marshal.ReadInt32 ((IntPtr)(pNativeData.ToInt32() + 4)))));

            case Scheme_Type.Symbol_Type:
              return Marshal.PtrToStringAnsi ((IntPtr)(pNativeData.ToInt32() + 8));

            case Scheme_Type.True_Type:
              return true;

            default:
              this.handle = handle;
              return this;
              }
          }
    }

    static public ICustomMarshaler GetInstance (String name)
    {
      Console.WriteLine ("GetInstance (" + name.ToString() + ")");
      Console.Out.Flush();
      return new MzSchemeObject();
    }

  }

  [
      ComSourceInterfaces ("SchemeBridge.ISink")
      ]
  public class Helper
  {
    System.Reflection.Assembly forms_assembly;
    System.Reflection.Assembly drawing_assembly;
    System.Reflection.Assembly excel_assembly;

    public event Callback0Delegate Callback0;
    public event Callback1Delegate Callback1;

    public void TriggerRandomEvent ()
    {
      // Console.Write ("x");
      // Console.Out.Flush();
      if (Callback0 != null)
	  Callback0 ("Bar");
    }

    public void TriggerAnotherEvent ()
    {
      if (Callback1 != null)
	  Callback1 ("Foo", System.Threading.Thread.CurrentThread);
    }

    public static void ReportError (String where, Exception e)
    {
      Console.WriteLine ("CLR error " + where);
      Console.WriteLine ("Message is:  " + e.Message);
      Console.WriteLine ("Stack trace follows");
      Console.WriteLine ("--------");
      Console.WriteLine (e.StackTrace);
      Console.WriteLine ("--------");
      if (e.InnerException != null) {
          Console.WriteLine ("Inner exception present.");
          Console.WriteLine (e.InnerException);
          Console.WriteLine ("");
          }
      Console.WriteLine ("");
      Console.Out.Flush();
      throw e;
    }

    public Object ConstructorCall (ConstructorInfo meth, BridgeArray args)
    {
      try {
	Object [] converted_args = new Object [meth.GetParameters().Length];
	for (int i = 0; i < args.Length; i++) {
	    Type actual_type = args[i].GetType();
          ParameterInfo paraminfo = meth.GetParameters()[i];
          ParameterAttributes paramattr = paraminfo.Attributes;
          Type desired_type = meth.GetParameters()[i].ParameterType;
	    if (actual_type.Equals (desired_type) ||
	        actual_type.IsSubclassOf (desired_type))
                converted_args [i] = args [i];
	  else if (desired_type.IsValueType && actual_type.IsValueType) {
                Console.WriteLine ("Have to convert value types or something.");
                }
          else if (desired_type.IsEnum &&
                   args[i] is System.String)
              converted_args [i] = System.Enum.Parse (desired_type, (System.String)(args[i]));
          else if ((paramattr & ParameterAttributes.HasFieldMarshal) == paramattr) {
	      Console.WriteLine ("Parameter " + i + " HasFieldMarshal");
	      Console.WriteLine ("actual type is " + actual_type);
	      Console.WriteLine ("desired type is " + desired_type);

              Object [] invoke_args = new Object [1];
              invoke_args [0] = "hello";
              MethodInfo getinstancemethod = desired_type.GetMethod ("GetInstance");
	      if (getinstancemethod == null)
                 Console.WriteLine ("GetInstance method is NULL, oh my!");

	      Console.WriteLine ("GetInstance method is " + getinstancemethod.ToString());
              ICustomMarshaler marshaler = (ICustomMarshaler) (getinstancemethod.Invoke (null, invoke_args));
              Object marshaled =  marshaler.MarshalNativeToManaged ((System.IntPtr)(args [i]));
              converted_args [i] = marshaled;
              }
          else {
              converted_args [i] = args [i];
              }
          }

      for (int i = args.Length; i < meth.GetParameters().Length; i++)
          converted_args [i] = meth.GetParameters()[i].DefaultValue;


	Console.WriteLine ("About to call constructor on:");
          foreach (Object arg in converted_args)
              Console.WriteLine ("Arg: " + arg.ToString());

	  return BridgeWrapper.Wrap (meth.Invoke (converted_args));
	  }
      catch (Exception e) {
          ReportError ("constructor invocation.", e);
          return null;
          }
    }

    public Object GetPropertyValue (PropertyInfo pi, Object instance, BridgeArray indexes)
    {
      try {
          return BridgeWrapper.Wrap (pi.GetValue ((instance is BridgeWrapper)
                                                  ? ((BridgeWrapper)instance).underlying_object
                                                  : instance, indexes));
          }
      catch (Exception e) {
          ReportError ("property value.", e);
          return null;
          }
    }

    public void SetPropertyValue (PropertyInfo pi, Object instance, Object new_value, BridgeArray indexes)
    {
	MethodInfo info = pi.GetSetMethod();

      Object [] converted_args = new Object [info.GetParameters().Length];

      for (int i = 0; i < indexes.Length; i++) {
          Type actual_type = indexes[i].GetType();
          ParameterInfo paraminfo = info.GetParameters()[i];
          ParameterAttributes paramattr = paraminfo.Attributes;
          Type desired_type = info.GetParameters()[i].ParameterType;

	      Console.WriteLine ("Parameter " + i);
	      Console.WriteLine ("actual type is " + actual_type);
	      Console.WriteLine ("desired type is " + desired_type);

          if (actual_type.Equals (desired_type) ||
              actual_type.IsSubclassOf (desired_type))
              converted_args [i] = indexes [i];
          else if (desired_type.IsEnum &&
                   indexes[i] is System.String) {


              converted_args [i] = System.Enum.Parse (desired_type, (System.String)(indexes[i]));
              }
          else if ((paramattr & ParameterAttributes.HasFieldMarshal) == paramattr) {
              Object [] invoke_args = new Object [1];
              invoke_args [0] = "hello";
              MethodInfo getinstancemethod = desired_type.GetMethod ("GetInstance");
              ICustomMarshaler marshaler = (ICustomMarshaler) (getinstancemethod.Invoke (null, invoke_args));
              Object marshaled =  marshaler.MarshalNativeToManaged ((System.IntPtr)(indexes [i]));
              converted_args [i] = marshaled;
              }
          else {
              converted_args [i] = indexes [i];
              }
          }

      for (int i = indexes.Length; i < info.GetParameters().Length; i++)
          converted_args [i] = info.GetParameters()[i].DefaultValue;

      Object nv = (new_value is BridgeWrapper)
                  ? ((BridgeWrapper) new_value).underlying_object
                  : new_value;

      Type desired_new_value_type = pi.PropertyType;
      Type actual_new_value_type = nv.GetType();

      Object new_value_object;
      if (actual_new_value_type.Equals (desired_new_value_type) ||
	  actual_new_value_type.IsSubclassOf (desired_new_value_type))
         new_value_object = nv;
      else if (desired_new_value_type.IsEnum &&
               nv is System.String)
         new_value_object = System.Enum.Parse (desired_new_value_type, (System.String)(nv));
      else
         new_value_object = nv;


      try {
          pi.SetValue ((instance is BridgeWrapper)
                       ? ((BridgeWrapper)instance).underlying_object
                       : instance,
                       new_value_object,
                       indexes.Length == 0
                       ? null
                       : converted_args);
          }
      catch (Exception e) {
          ReportError ("set property value.", e);
          }
    }

    public Object StaticCall (MethodInfo meth, BridgeArray args)
    {
      return DynamicCall (meth, null, args);
    }

    public Object DynamicCall (MethodInfo meth, Object instance, BridgeArray args)
    {
      Object [] converted_args = new Object [meth.GetParameters().Length];

      for (int i = 0; i < args.Length; i++) {
          Type actual_type = args[i].GetType();
          ParameterInfo paraminfo = meth.GetParameters()[i];
          ParameterAttributes paramattr = paraminfo.Attributes;
          Type desired_type = meth.GetParameters()[i].ParameterType;
          if (actual_type.Equals (desired_type) ||
              actual_type.IsSubclassOf (desired_type))
              converted_args [i] = args [i];
          else if (desired_type.IsEnum &&
                   args[i] is System.String)
              converted_args [i] = System.Enum.Parse (desired_type, (System.String)(args[i]));
          else if ((paramattr & ParameterAttributes.HasFieldMarshal) == paramattr) {
              Object [] invoke_args = new Object [1];
              invoke_args [0] = "hello";
              MethodInfo getinstancemethod = desired_type.GetMethod ("GetInstance");
              ICustomMarshaler marshaler = (ICustomMarshaler) (getinstancemethod.Invoke (null, invoke_args));
              Object marshaled =  marshaler.MarshalNativeToManaged ((System.IntPtr)(args [i]));
              converted_args [i] = marshaled;
              }
          else {
              converted_args [i] = args [i];
              }
          }

      for (int i = args.Length; i < meth.GetParameters().Length; i++)
          converted_args [i] = meth.GetParameters()[i].DefaultValue;

      try {
          // UGH!  Can't wrap voids!
          return (meth.ReturnType == typeof (void))
              ? meth.Invoke ((instance is BridgeWrapper)
                                                  ? ((BridgeWrapper)instance).underlying_object
                                                  : instance, converted_args)
              : BridgeWrapper.Wrap (meth.Invoke ((instance is BridgeWrapper)
                                                  ? ((BridgeWrapper)instance).underlying_object
                                                  : instance, converted_args));
	  }
      catch (Exception e) {
          foreach (Object arg in converted_args)
              Console.WriteLine ("Arg: " + arg.ToString());

          ReportError ("method invocation of " + meth.ToString(), e);
          return null;
	  }
    }

    public Type FindType (String typename)
    {
      // case insensitive, but throw errors
      try {
	  return System.Type.GetType (typename, true, true);
	  }
      catch (TypeLoadException) {
	  try {
	      return forms_assembly.GetType (typename, true, true);
	      }
	  catch (TypeLoadException) {
	      try {
		  return drawing_assembly.GetType (typename, true, true);
		  }
	      catch (TypeLoadException) {
                  try {
                      return excel_assembly.GetType (typename, true, true);
                      }
                  catch (TypeLoadException e2) {
                      if (typename.CompareTo ("schemebridge.containercontrolproxy") == 0)
                          return typeof (ContainerControlProxy);
                      else if (typename.CompareTo ("schemebridge.formproxy") == 0)
                          return typeof (FormProxy);
                      else if (typename.CompareTo ("schemebridge.richtextproxy") == 0)
                          return typeof (RichTextProxy);
                      else if (typename.CompareTo ("schemebridge.applicationcontext") == 0)
                          return typeof (ApplicationContext);
                      else
                          Console.Write ("Couldn't load type " + e2.TypeName + "\n");
                      }
		  }
	      }
	  }
      return null;
    }

    public MemberInfo TypeMember (Type thing, int index)
    {
      return thing.GetMembers()[index];
    }

    public int TypeMemberCount (Type thing)
    {
      return thing.GetMembers().Length;
    }

    public void SetEntryPoint (AssemblyBuilder ab, ModuleBuilder mb, String typename, String name)
    {
      ab.SetEntryPoint ((mb.GetType (typename)).GetMethod (name));
    }

    public Object GetSTAThreadAttribute ()
    {
      return (Object)(new System.STAThreadAttribute());
    }

    public Helper()
    {
      forms_assembly = System.Reflection.Assembly.LoadWithPartialName ("System.Windows.Forms");
      drawing_assembly = System.Reflection.Assembly.LoadWithPartialName ("System.Drawing");
      excel_assembly = System.Reflection.Assembly.LoadWithPartialName ("Microsoft.Office.Interop.Excel");
    }

    public Object WhatsThis (Object thing)
    {
      Console.Write ("This appears to be:  " + thing.GetType().ToString() + " " + thing + "\n");
      return thing;
    }

    public Object testint ()
    {
      return Scheme.scheme_make_integer_value (3);
    }

    public Object teststring ()
    {
      return Scheme.scheme_make_string ("All your base are belong to us!");
    }

    public Object testptr ()
    {
      return Scheme.scheme_intern_symbol ("Skidoo");
    }

    public Object testcar (MzSchemeObject pair)
    {
      return Scheme.scheme_car (pair);
    }

    public MzSchemeObject testidentity (MzSchemeObject thing)
    {
      Console.WriteLine ("Returning " + thing.ToString());
      return thing;
    }

    public Object testprint (MzSchemeObject thing)
    {
      Console.WriteLine ("Calling scheme_debug_print on " + thing.ToString());
      Scheme.scheme_debug_print (thing);
      Console.WriteLine ("scheme_debug_print returned");
      return false;
    }

//     public Object TestOne (MzSchemeObject thunk)
//     {
//       return Scheme.scheme_apply (thunk, 0, null);
//     }

//     public Object testtwo (MzSchemeObject thunk)
//     {
//       Object [] args = new Object [1];
//       args [0] = 42;
//       return Scheme.scheme_apply (thunk, 1, args);
//     }

    public MzSchemeObject testcons (MzSchemeObject car, MzSchemeObject cdr)
    {
      return Scheme.scheme_car (Scheme.scheme_make_pair (car, cdr));
    }

    public Object testthree (MzSchemeObject thunk)
    {
      // System.Diagnostics.Debugger.Break();
      Object [] args = new Object [2];
      args [0] = 23;
      args [1] = "skidoo";

      IntPtr buffer = Marshal.AllocCoTaskMem (args.Length * 4);
      MzSchemeObject marshaler = new MzSchemeObject();

      for (int i = 0; i < 2; i++)
          Marshal.WriteIntPtr (buffer, i * 4, marshaler.MarshalManagedToNative (args [i]));

      // Marshal.Copy (args, 0, buffer, args.Length);

      Object result = Scheme.scheme_apply (thunk, 2, buffer);
      Marshal.FreeCoTaskMem (buffer);

      return result;
    }

    public void DoEvent (String event_name, System.Windows.Forms.Form form, System.EventArgs evnt)
    {
      Console.WriteLine (event_name);
      return;
    }

  }

  public class FakeILGenerator
  {
    public ILGenerator realILGenerator;

    public FakeILGenerator (ILGenerator arg)
    {
      realILGenerator = arg;
    }

    public void BeginCatchBlock (Type exception_type)
    {
      realILGenerator.BeginCatchBlock (exception_type);
    }

    public void BeginExceptFilterBlock ()
    {
      realILGenerator.BeginExceptFilterBlock ();
    }

    public FakeLabel BeginExceptionBlock ()
    {
      return new FakeLabel (realILGenerator.BeginExceptionBlock ());
    }

    public void BeginFaultBlock ()
    {
      realILGenerator.BeginFaultBlock ();
    }

    public void BeginFinallyBlock ()
    {
      realILGenerator.BeginFinallyBlock ();
    }

    public void BeginScope ()
    {
      realILGenerator.BeginScope ();
    }

    public LocalBuilder DeclareLocal (Type localType)
    {
      return realILGenerator.DeclareLocal (localType);
    }

    public FakeLabel DefineLabel ()
    {
      return new FakeLabel (realILGenerator.DefineLabel());
    }

    public void EmitCall0 (String name, MethodInfo mi)
    {
      if (name.CompareTo ("Call") == 0) realILGenerator.EmitCall (OpCodes.Call, mi, null);
      else if (name.CompareTo ("CallVirt") == 0) realILGenerator.EmitCall (OpCodes.Callvirt, mi, null);
    }

    public void EmitCall (String name, MethodInfo mi, Object [] types)
    {
      Type [] pt = new Type [types.Length];
      types.CopyTo (pt, 0);

      if (name.CompareTo ("Call") == 0) realILGenerator.EmitCall (OpCodes.Call, mi, pt);
      else if (name.CompareTo ("CallVirt") == 0) realILGenerator.EmitCall (OpCodes.Callvirt, mi, pt);
    }

    public void EmitCalli (String name,
			   System.Runtime.InteropServices.CallingConvention cc,
			   Type returnType,
			   BridgeArray paramtypes)
    {
      if (name.CompareTo ("Calli") == 0) realILGenerator.EmitCalli (OpCodes.Calli, cc, returnType, (Type[])paramtypes);
    }

    public void EmitCalliOpt (String name, System.Reflection.CallingConventions cc, Type returnType,
			      BridgeArray paramtypes,
			      BridgeArray optTypes)
    {
      if (name.CompareTo ("Calli") == 0)
	  realILGenerator.EmitCalli (OpCodes.Calli, cc, returnType, (Type[])paramtypes, (Type[])optTypes);
    }

    public void EmitWriteLineField (FieldInfo fi)
    {
      realILGenerator.EmitWriteLine (fi);
    }

    public void EmitWriteLineLocalBuilder (LocalBuilder lb)
    {
      realILGenerator.EmitWriteLine (lb);
    }

    public void EmitWriteLineString (String str)
    {
      realILGenerator.EmitWriteLine (str);
    }

    public void Emit (String name)
    {
      if (name.CompareTo ("Add") == 0) realILGenerator.Emit (OpCodes.Add);
      else if (name.CompareTo ("Add_Ovf") == 0) realILGenerator.Emit (OpCodes.Add_Ovf);
      else if (name.CompareTo ("Add_Ovf_Un") == 0) realILGenerator.Emit (OpCodes.Add_Ovf_Un);
      else if (name.CompareTo ("And") == 0) realILGenerator.Emit (OpCodes.And);
      else if (name.CompareTo ("Arglist") == 0) realILGenerator.Emit (OpCodes.Arglist);
      else if (name.CompareTo ("Break") == 0) realILGenerator.Emit (OpCodes.Break);
      else if (name.CompareTo ("Ceq") == 0) realILGenerator.Emit (OpCodes.Ceq);
      else if (name.CompareTo ("Cgt") == 0) realILGenerator.Emit (OpCodes.Cgt);
      else if (name.CompareTo ("Cgt_Un") == 0) realILGenerator.Emit (OpCodes.Cgt_Un);
      else if (name.CompareTo ("Ckfinite") == 0) realILGenerator.Emit (OpCodes.Ckfinite);
      else if (name.CompareTo ("Clt") == 0) realILGenerator.Emit (OpCodes.Clt);
      else if (name.CompareTo ("Clt_Un") == 0) realILGenerator.Emit (OpCodes.Clt_Un);
      else if (name.CompareTo ("Conv_I") == 0) realILGenerator.Emit (OpCodes.Conv_I);
      else if (name.CompareTo ("Conv_I1") == 0) realILGenerator.Emit (OpCodes.Conv_I1);
      else if (name.CompareTo ("Conv_I2") == 0) realILGenerator.Emit (OpCodes.Conv_I2);
      else if (name.CompareTo ("Conv_I4") == 0) realILGenerator.Emit (OpCodes.Conv_I4);
      else if (name.CompareTo ("Conv_I8") == 0) realILGenerator.Emit (OpCodes.Conv_I8);
      else if (name.CompareTo ("Conv_Ovf_I") == 0) realILGenerator.Emit (OpCodes.Conv_Ovf_I);
      else if (name.CompareTo ("Conv_Ovf_I1") == 0) realILGenerator.Emit (OpCodes.Conv_Ovf_I1);
      else if (name.CompareTo ("Conv_Ovf_I1_Un") == 0) realILGenerator.Emit (OpCodes.Conv_Ovf_I1_Un);
      else if (name.CompareTo ("Conv_Ovf_I2") == 0) realILGenerator.Emit (OpCodes.Conv_Ovf_I2);
      else if (name.CompareTo ("Conv_Ovf_I2_Un") == 0) realILGenerator.Emit (OpCodes.Conv_Ovf_I2_Un);
      else if (name.CompareTo ("Conv_Ovf_I4") == 0) realILGenerator.Emit (OpCodes.Conv_Ovf_I4);
      else if (name.CompareTo ("Conv_Ovf_I4_Un") == 0) realILGenerator.Emit (OpCodes.Conv_Ovf_I4_Un);
      else if (name.CompareTo ("Conv_Ovf_I8") == 0) realILGenerator.Emit (OpCodes.Conv_Ovf_I8);
      else if (name.CompareTo ("Conv_Ovf_I8_Un") == 0) realILGenerator.Emit (OpCodes.Conv_Ovf_I8_Un);
      else if (name.CompareTo ("Conv_Ovf_I_Un") == 0) realILGenerator.Emit (OpCodes.Conv_Ovf_I_Un);
      else if (name.CompareTo ("Conv_Ovf_U") == 0) realILGenerator.Emit (OpCodes.Conv_Ovf_U);
      else if (name.CompareTo ("Conv_Ovf_U1") == 0) realILGenerator.Emit (OpCodes.Conv_Ovf_U1);
      else if (name.CompareTo ("Conv_Ovf_U1_Un") == 0) realILGenerator.Emit (OpCodes.Conv_Ovf_U1_Un);
      else if (name.CompareTo ("Conv_Ovf_U2") == 0) realILGenerator.Emit (OpCodes.Conv_Ovf_U2);
      else if (name.CompareTo ("Conv_Ovf_U2_Un") == 0) realILGenerator.Emit (OpCodes.Conv_Ovf_U2_Un);
      else if (name.CompareTo ("Conv_Ovf_U4") == 0) realILGenerator.Emit (OpCodes.Conv_Ovf_U4);
      else if (name.CompareTo ("Conv_Ovf_U4_Un") == 0) realILGenerator.Emit (OpCodes.Conv_Ovf_U4_Un);
      else if (name.CompareTo ("Conv_Ovf_U8") == 0) realILGenerator.Emit (OpCodes.Conv_Ovf_U8);
      else if (name.CompareTo ("Conv_Ovf_U8_Un") == 0) realILGenerator.Emit (OpCodes.Conv_Ovf_U8_Un);
      else if (name.CompareTo ("Conv_Ovf_U_Un") == 0) realILGenerator.Emit (OpCodes.Conv_Ovf_U_Un);
      else if (name.CompareTo ("Conv_R4") == 0) realILGenerator.Emit (OpCodes.Conv_R4);
      else if (name.CompareTo ("Conv_R8") == 0) realILGenerator.Emit (OpCodes.Conv_R8);
      else if (name.CompareTo ("Conv_R_Un") == 0) realILGenerator.Emit (OpCodes.Conv_R_Un);
      else if (name.CompareTo ("Conv_U") == 0) realILGenerator.Emit (OpCodes.Conv_U);
      else if (name.CompareTo ("Conv_U1") == 0) realILGenerator.Emit (OpCodes.Conv_U1);
      else if (name.CompareTo ("Conv_U2") == 0) realILGenerator.Emit (OpCodes.Conv_U2);
      else if (name.CompareTo ("Conv_U4") == 0) realILGenerator.Emit (OpCodes.Conv_U4);
      else if (name.CompareTo ("Conv_U8") == 0) realILGenerator.Emit (OpCodes.Conv_U8);
      else if (name.CompareTo ("Cpblk") == 0) realILGenerator.Emit (OpCodes.Cpblk);
      else if (name.CompareTo ("Div") == 0) realILGenerator.Emit (OpCodes.Div);
      else if (name.CompareTo ("Div_Un") == 0) realILGenerator.Emit (OpCodes.Div_Un);
      else if (name.CompareTo ("Dup") == 0) realILGenerator.Emit (OpCodes.Dup);
      else if (name.CompareTo ("Endfilter") == 0) realILGenerator.Emit (OpCodes.Endfilter);
      else if (name.CompareTo ("Endfinally") == 0) realILGenerator.Emit (OpCodes.Endfinally);
      else if (name.CompareTo ("Initblk") == 0) realILGenerator.Emit (OpCodes.Initblk);
      else if (name.CompareTo ("Ldarg_0") == 0) realILGenerator.Emit (OpCodes.Ldarg_0);
      else if (name.CompareTo ("Ldarg_1") == 0) realILGenerator.Emit (OpCodes.Ldarg_1);
      else if (name.CompareTo ("Ldarg_2") == 0) realILGenerator.Emit (OpCodes.Ldarg_2);
      else if (name.CompareTo ("Ldarg_3") == 0) realILGenerator.Emit (OpCodes.Ldarg_3);
      else if (name.CompareTo ("Ldc_I4_0") == 0) realILGenerator.Emit (OpCodes.Ldc_I4_0);
      else if (name.CompareTo ("Ldc_I4_1") == 0) realILGenerator.Emit (OpCodes.Ldc_I4_1);
      else if (name.CompareTo ("Ldc_I4_2") == 0) realILGenerator.Emit (OpCodes.Ldc_I4_2);
      else if (name.CompareTo ("Ldc_I4_3") == 0) realILGenerator.Emit (OpCodes.Ldc_I4_3);
      else if (name.CompareTo ("Ldc_I4_4") == 0) realILGenerator.Emit (OpCodes.Ldc_I4_4);
      else if (name.CompareTo ("Ldc_I4_5") == 0) realILGenerator.Emit (OpCodes.Ldc_I4_5);
      else if (name.CompareTo ("Ldc_I4_6") == 0) realILGenerator.Emit (OpCodes.Ldc_I4_6);
      else if (name.CompareTo ("Ldc_I4_7") == 0) realILGenerator.Emit (OpCodes.Ldc_I4_7);
      else if (name.CompareTo ("Ldc_I4_8") == 0) realILGenerator.Emit (OpCodes.Ldc_I4_8);
      else if (name.CompareTo ("Ldc_I4_M1") == 0) realILGenerator.Emit (OpCodes.Ldc_I4_M1);
      else if (name.CompareTo ("Ldelem_I") == 0) realILGenerator.Emit (OpCodes.Ldelem_I);
      else if (name.CompareTo ("Ldelem_I1") == 0) realILGenerator.Emit (OpCodes.Ldelem_I1);
      else if (name.CompareTo ("Ldelem_I2") == 0) realILGenerator.Emit (OpCodes.Ldelem_I2);
      else if (name.CompareTo ("Ldelem_I4") == 0) realILGenerator.Emit (OpCodes.Ldelem_I4);
      else if (name.CompareTo ("Ldelem_I8") == 0) realILGenerator.Emit (OpCodes.Ldelem_I8);
      else if (name.CompareTo ("Ldelem_R4") == 0) realILGenerator.Emit (OpCodes.Ldelem_R4);
      else if (name.CompareTo ("Ldelem_R8") == 0) realILGenerator.Emit (OpCodes.Ldelem_R8);
      else if (name.CompareTo ("Ldelem_Ref") == 0) realILGenerator.Emit (OpCodes.Ldelem_Ref);
      else if (name.CompareTo ("Ldelem_U1") == 0) realILGenerator.Emit (OpCodes.Ldelem_U1);
      else if (name.CompareTo ("Ldelem_U2") == 0) realILGenerator.Emit (OpCodes.Ldelem_U2);
      else if (name.CompareTo ("Ldelem_U4") == 0) realILGenerator.Emit (OpCodes.Ldelem_U4);
      else if (name.CompareTo ("Ldind_I") == 0) realILGenerator.Emit (OpCodes.Ldind_I);
      else if (name.CompareTo ("Ldind_I1") == 0) realILGenerator.Emit (OpCodes.Ldind_I1);
      else if (name.CompareTo ("Ldind_I2") == 0) realILGenerator.Emit (OpCodes.Ldind_I2);
      else if (name.CompareTo ("Ldind_I4") == 0) realILGenerator.Emit (OpCodes.Ldind_I4);
      else if (name.CompareTo ("Ldind_I8") == 0) realILGenerator.Emit (OpCodes.Ldind_I8);
      else if (name.CompareTo ("Ldind_R4") == 0) realILGenerator.Emit (OpCodes.Ldind_R4);
      else if (name.CompareTo ("Ldind_R8") == 0) realILGenerator.Emit (OpCodes.Ldind_R8);
      else if (name.CompareTo ("Ldind_Ref") == 0) realILGenerator.Emit (OpCodes.Ldind_Ref);
      else if (name.CompareTo ("Ldind_U1") == 0) realILGenerator.Emit (OpCodes.Ldind_U1);
      else if (name.CompareTo ("Ldind_U2") == 0) realILGenerator.Emit (OpCodes.Ldind_U2);
      else if (name.CompareTo ("Ldind_U4") == 0) realILGenerator.Emit (OpCodes.Ldind_U4);
      else if (name.CompareTo ("Ldlen") == 0) realILGenerator.Emit (OpCodes.Ldlen);
      else if (name.CompareTo ("Ldloc_0") == 0) realILGenerator.Emit (OpCodes.Ldloc_0);
      else if (name.CompareTo ("Ldloc_1") == 0) realILGenerator.Emit (OpCodes.Ldloc_1);
      else if (name.CompareTo ("Ldloc_2") == 0) realILGenerator.Emit (OpCodes.Ldloc_2);
      else if (name.CompareTo ("Ldloc_3") == 0) realILGenerator.Emit (OpCodes.Ldloc_3);
      else if (name.CompareTo ("Ldnull") == 0) realILGenerator.Emit (OpCodes.Ldnull);
      else if (name.CompareTo ("Localloc") == 0) realILGenerator.Emit (OpCodes.Localloc);
      else if (name.CompareTo ("Mul") == 0) realILGenerator.Emit (OpCodes.Mul);
      else if (name.CompareTo ("Mul_Ovf") == 0) realILGenerator.Emit (OpCodes.Mul_Ovf);
      else if (name.CompareTo ("Mul_Ovf_Un") == 0) realILGenerator.Emit (OpCodes.Mul_Ovf_Un);
      else if (name.CompareTo ("Neg") == 0) realILGenerator.Emit (OpCodes.Neg);
      else if (name.CompareTo ("Nop") == 0) realILGenerator.Emit (OpCodes.Nop      );
      else if (name.CompareTo ("Not") == 0) realILGenerator.Emit (OpCodes.Not      );
      else if (name.CompareTo ("Or") == 0) realILGenerator.Emit (OpCodes.Or      );
      else if (name.CompareTo ("Pop") == 0) realILGenerator.Emit (OpCodes.Pop);
      else if (name.CompareTo ("Refanytype") == 0) realILGenerator.Emit (OpCodes.Refanytype);
      else if (name.CompareTo ("Rem") == 0) realILGenerator.Emit (OpCodes.Rem);
      else if (name.CompareTo ("Rem_Un") == 0) realILGenerator.Emit (OpCodes.Rem_Un);
      else if (name.CompareTo ("Ret") == 0) realILGenerator.Emit (OpCodes.Ret);
      else if (name.CompareTo ("Rethrow") == 0) realILGenerator.Emit (OpCodes.Rethrow);
      else if (name.CompareTo ("Shl") == 0) realILGenerator.Emit (OpCodes.Shl);
      else if (name.CompareTo ("Shr") == 0) realILGenerator.Emit (OpCodes.Shr);
      else if (name.CompareTo ("Shr_Un") == 0) realILGenerator.Emit (OpCodes.Shr_Un);
      else if (name.CompareTo ("Stelem_I1") == 0) realILGenerator.Emit (OpCodes.Stelem_I1);
      else if (name.CompareTo ("Stelem_I2") == 0) realILGenerator.Emit (OpCodes.Stelem_I2);
      else if (name.CompareTo ("Stelem_I4") == 0) realILGenerator.Emit (OpCodes.Stelem_I4);
      else if (name.CompareTo ("Stelem_I8") == 0) realILGenerator.Emit (OpCodes.Stelem_I8);
      else if (name.CompareTo ("Stelem_R4") == 0) realILGenerator.Emit (OpCodes.Stelem_R4);
      else if (name.CompareTo ("Stelem_R8") == 0) realILGenerator.Emit (OpCodes.Stelem_R8);
      else if (name.CompareTo ("Stelem_Ref") == 0) realILGenerator.Emit (OpCodes.Stelem_Ref);
      else if (name.CompareTo ("Stind_I1") == 0) realILGenerator.Emit (OpCodes.Stind_I1);
      else if (name.CompareTo ("Stind_I2") == 0) realILGenerator.Emit (OpCodes.Stind_I2);
      else if (name.CompareTo ("Stind_I4") == 0) realILGenerator.Emit (OpCodes.Stind_I4);
      else if (name.CompareTo ("Stind_I8") == 0) realILGenerator.Emit (OpCodes.Stind_I8);
      else if (name.CompareTo ("Stind_R4") == 0) realILGenerator.Emit (OpCodes.Stind_R4);
      else if (name.CompareTo ("Stind_R8") == 0) realILGenerator.Emit (OpCodes.Stind_R8);
      else if (name.CompareTo ("Stind_Ref") == 0) realILGenerator.Emit (OpCodes.Stind_Ref);
      else if (name.CompareTo ("Stloc_0") == 0) realILGenerator.Emit (OpCodes.Stloc_0);
      else if (name.CompareTo ("Stloc_1") == 0) realILGenerator.Emit (OpCodes.Stloc_1);
      else if (name.CompareTo ("Stloc_2") == 0) realILGenerator.Emit (OpCodes.Stloc_2);
      else if (name.CompareTo ("Stloc_3") == 0) realILGenerator.Emit (OpCodes.Stloc_3);
      else if (name.CompareTo ("Sub") == 0) realILGenerator.Emit (OpCodes.Sub);
      else if (name.CompareTo ("Sub_Ovf") == 0) realILGenerator.Emit (OpCodes.Sub_Ovf);
      else if (name.CompareTo ("Sub_Ovf_Un") == 0) realILGenerator.Emit (OpCodes.Sub_Ovf_Un);
      else if (name.CompareTo ("Tailcall") == 0) realILGenerator.Emit (OpCodes.Tailcall);
      else if (name.CompareTo ("Throw") == 0) realILGenerator.Emit (OpCodes.Throw);
      else if (name.CompareTo ("Volatile") == 0) realILGenerator.Emit (OpCodes.Volatile);
      else if (name.CompareTo ("Xor") == 0) realILGenerator.Emit (OpCodes.Xor);
    }

    public void EmitByte (String name, int value)
    {
    }

    public void EmitConstructorBuilder (String name, FakeConstructorBuilder ci)
    {
      if (name.CompareTo ("Newobj") == 0) realILGenerator.Emit (OpCodes.Newobj, ci.realConstructorBuilder);
    }

    public void EmitConstructorInfo (String name, ConstructorInfo ci)
    {
      if (name.CompareTo ("Call") == 0) realILGenerator.Emit (OpCodes.Call, ci);
    }

    public void EmitDouble (String name, Double value)
    {
    }

    public void EmitFieldInfo (String name, FieldInfo fi)
    {
      if (name.CompareTo ("Ldfld") == 0) realILGenerator.Emit (OpCodes.Ldfld, fi);
      else if (name.CompareTo ("Stfld") == 0) realILGenerator.Emit (OpCodes.Stfld, fi);
    }

    public void EmitInt16 (String name, int value)
    {
    }

    public void EmitInt32 (String name, int value)
    {
      if (name.CompareTo ("Ldc_I4") == 0) realILGenerator.Emit (OpCodes.Ldc_I4, value);
    }

    public void EmitInt64 (String name, int value)
    {
    }

    public void EmitLabel (String name, FakeLabel label)
    {
      if (name.CompareTo ("Beq") == 0) realILGenerator.Emit (OpCodes.Beq, label.realLabel);
      else if (name.CompareTo ("Beq_S") == 0) realILGenerator.Emit (OpCodes.Beq_S, label.realLabel);
      else if (name.CompareTo ("Bge") == 0) realILGenerator.Emit (OpCodes.Bge, label.realLabel);
      else if (name.CompareTo ("Bge_S") == 0) realILGenerator.Emit (OpCodes.Bge_S, label.realLabel);
      else if (name.CompareTo ("Bge_Un") == 0) realILGenerator.Emit (OpCodes.Bge_Un, label.realLabel);
      else if (name.CompareTo ("Bge_Un_S") == 0) realILGenerator.Emit (OpCodes.Bge_Un_S, label.realLabel);
      else if (name.CompareTo ("Bgt") == 0) realILGenerator.Emit (OpCodes.Bgt, label.realLabel);
      else if (name.CompareTo ("Bgt_S") == 0) realILGenerator.Emit (OpCodes.Bgt_S, label.realLabel);
      else if (name.CompareTo ("Bgt_Un") == 0) realILGenerator.Emit (OpCodes.Bgt_Un, label.realLabel);
      else if (name.CompareTo ("Bgt_Un_S") == 0) realILGenerator.Emit (OpCodes.Bgt_Un_S, label.realLabel);
      else if (name.CompareTo ("Ble") == 0) realILGenerator.Emit (OpCodes.Ble, label.realLabel);
      else if (name.CompareTo ("Ble_S") == 0) realILGenerator.Emit (OpCodes.Ble_S, label.realLabel);
      else if (name.CompareTo ("Ble_Un") == 0) realILGenerator.Emit (OpCodes.Ble_Un, label.realLabel);
      else if (name.CompareTo ("Ble_Un_S") == 0) realILGenerator.Emit (OpCodes.Ble_Un_S, label.realLabel);
      else if (name.CompareTo ("Blt") == 0) realILGenerator.Emit (OpCodes.Blt, label.realLabel);
      else if (name.CompareTo ("Blt_S") == 0) realILGenerator.Emit (OpCodes.Blt_S, label.realLabel);
      else if (name.CompareTo ("Blt_Un") == 0) realILGenerator.Emit (OpCodes.Blt_Un, label.realLabel);
      else if (name.CompareTo ("Blt_Un_S") == 0) realILGenerator.Emit (OpCodes.Blt_Un_S, label.realLabel);
      else if (name.CompareTo ("Bne_Un") == 0) realILGenerator.Emit (OpCodes.Bne_Un, label.realLabel);
      else if (name.CompareTo ("Bne_Un_S") == 0) realILGenerator.Emit (OpCodes.Bne_Un_S, label.realLabel);
      else if (name.CompareTo ("Br") == 0) realILGenerator.Emit (OpCodes.Br, label.realLabel);
      else if (name.CompareTo ("Brfalse") == 0) realILGenerator.Emit (OpCodes.Brfalse, label.realLabel);
      else if (name.CompareTo ("Brfalse_S") == 0) realILGenerator.Emit (OpCodes.Brfalse_S, label.realLabel);
      else if (name.CompareTo ("Br_S") == 0) realILGenerator.Emit (OpCodes.Br_S, label.realLabel);
      else if (name.CompareTo ("Leave") == 0) realILGenerator.Emit (OpCodes.Leave, label.realLabel);
      else if (name.CompareTo ("Leave_S") == 0) realILGenerator.Emit (OpCodes.Leave_S, label.realLabel);
    }


    public void EmitLocalBuilder (String name, LocalBuilder lb)
    {
    }

    public void EmitMethodInfo (String name, MethodInfo mi)
    {
    }

    public void EmitSByte (String name, int sb)
    {
    }

    public void EmitSignatureHelper (String name, SignatureHelper sh)
    {
    }

    public void EmitSingle (String name, Single s)
    {
    }

    public void EmitString (String name, String str)
    {
      if (name.CompareTo ("Ldstr") == 0) realILGenerator.Emit (OpCodes.Ldstr, str);
    }

    public void EmitType (String name, Type type)
    {
    }

    public void EndExceptionBlock ()
    {
      realILGenerator.EndExceptionBlock();
    }

    public void EndScope ()
    {
      realILGenerator.EndScope();
    }

    public void MarkLabel (FakeLabel fl)
    {
      realILGenerator.MarkLabel (fl.realLabel);
    }

    public void MarkSequencePoint (ISymbolDocumentWriter document,
				   int startLine,
				   int startColumn,
				   int endLine,
				   int endColumn)
    {
      realILGenerator.MarkSequencePoint (document, startLine, startColumn, endLine, endColumn);
    }

    public void ThrowException (Type excType)
    {
      realILGenerator.ThrowException (excType);
    }

    public void UsingNamespace (String usingNamespace)
    {
      realILGenerator.UsingNamespace (usingNamespace);
    }

  }

  public class FakeLabel
  {
    public System.Reflection.Emit.Label realLabel;

    public FakeLabel (System.Reflection.Emit.Label rl)
    {
      realLabel = rl;
    }
  }

  public class FakeConstructorBuilder
  {
    public ConstructorBuilder realConstructorBuilder;

    public FakeConstructorBuilder (ConstructorBuilder cb)
    {
      realConstructorBuilder = cb;
    }

    public FakeILGenerator GetILGenerator ()
    {
      return new FakeILGenerator (realConstructorBuilder.GetILGenerator());
    }
  }

  public class FakeMethodBuilder
  {
    public MethodBuilder realMethodBuilder;

    public FakeMethodBuilder (MethodBuilder mb)
    {
      realMethodBuilder = mb;
    }

    public FakeILGenerator GetILGenerator ()
    {
      return new FakeILGenerator (realMethodBuilder.GetILGenerator());
    }
  }

  public class FakeTypeBuilder
  {
    public ModuleBuilder mb;
    public TypeBuilder tb;

    public FakeConstructorBuilder DefineConstructor (MethodAttributes attributes,
						     CallingConventions callingConventions,
						     Object [] parameterTypes)
    {
      Type [] pt = new Type [parameterTypes.Length];
      parameterTypes.CopyTo (pt, 0);

      return new FakeConstructorBuilder
	  (tb.DefineConstructor (attributes, callingConventions, pt));
    }

    public void DefineType (ModuleBuilder argmb,
			    String name,
			    TypeAttributes attr,
			    Type parent,
			    Object [] interfaces_arg)
    {
      mb = argmb;

      Type [] interfaces = new Type [interfaces_arg.Length];
      interfaces_arg.CopyTo (interfaces, 0);

      tb = mb.DefineType (name, attr, parent, interfaces);
    }

    public FakeMethodBuilder DefineMethod (String name, MethodAttributes attr, Type returnType, Object[] paramtypes)
    {
      Type [] tp = new Type [paramtypes.Length];
      paramtypes.CopyTo (tp, 0);

      return new FakeMethodBuilder
	  (tb.DefineMethod (name,
			    attr,
			    returnType,
			    tp));
    }

    public FieldBuilder DefineField (String name, Type type, FieldAttributes attributes)
    {
      return tb.DefineField (name, type, attributes);
    }

    public Type CreateType ()
    {
      return tb.CreateType();
    }

    public FakeTypeBuilder ()
    {
    }

  }

// Since Color is a Value type rather than a class type,
// we wrap it in a class.

public class ColorClass {

  public System.Drawing.Color realColor;

  public ColorClass (int r, int g, int b) {
    realColor = System.Drawing.Color.FromArgb (r, g, b);
    }

  public ColorClass (String name) {
    realColor = System.Drawing.Color.FromName (name);
    }

  public static implicit operator System.Drawing.Color (ColorClass source)
  {
    return source.realColor;
  }

  public void Set (int r, int g, int b) {
    this.realColor = System.Drawing.Color.FromArgb (r, g, b);
    }

  public int Red
  {
    get {
        return realColor.R;
        }
  }

  public int Green
  {
    get {
        return realColor.G;
        }
  }

  public int Blue
  {
    get {
        return realColor.B;
        }
  }
}

// It seems that the system cursor collection is bogusly typed.

public class FakeCursor {
   public System.Windows.Forms.Cursor realCursor;

   public FakeCursor (System.Windows.Forms.Cursor _realCursor) {
      realCursor = _realCursor;
      }
}

public class FakeCursorCollection {
   public static Object Arrow () {return BridgeWrapper.Wrap (new FakeCursor (System.Windows.Forms.Cursors.Arrow));}
   public static Object Cross () {return BridgeWrapper.Wrap (new FakeCursor (System.Windows.Forms.Cursors.Cross));}
   public static Object Hand  () {return BridgeWrapper.Wrap (new FakeCursor (System.Windows.Forms.Cursors.Hand));}
   public static Object IBeam () {return BridgeWrapper.Wrap (new FakeCursor (System.Windows.Forms.Cursors.IBeam));}
}

public class FakePen {
    public System.Drawing.Pen realPen;

    public FakePen (System.Drawing.Pen _realPen) {
        realPen = _realPen;
        }

  public System.Drawing.Drawing2D.DashStyle dashStyle {
    get {
        return realPen.DashStyle;
        }

    set {
	// Another winner:  a `default' name for the argument!
        realPen.DashStyle = value;
        }
  }

}

public class GUIHelper {

  public static Object make_pen (ColorClass color, int width) {
    return new FakePen (new System.Drawing.Pen (color, width));
    }

}


//////////////////////////////////////
// As it turns out, there are certain metadata constructs that cannot
// be manipulated from within the .NET framework.  These have to do
// with events, and events are what I'm primarily interested in!
//
// In order to behave like an official .NET GUI object, you have to
// inherit from the .NET GUI hierarchy.  I can't create one of these
// objects from Scheme, so I have to create it here and use this as a
// proxy object back to Scheme.
/////////////////////////////////////

  public unsafe class ControlProxy : System.Windows.Forms.Control
  {
    public MzSchemeObject scheme_control;

    public ControlProxy (MzSchemeObject _scheme_control)
    {
      scheme_control = _scheme_control;
    }

  private void NotifyScheme (String event_name)
    {
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (4);
          Marshal.WriteIntPtr (buffer, 0, scheme_control.MarshalManagedToNative (event_name));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_control, 1, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, EventArgs e)
    {
      NotifyScheme (event_name);
    }

    private void NotifyScheme (String event_name, MouseEventArgs e)
    {
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (4 * 6);
          Marshal.WriteIntPtr (buffer, 0, scheme_control.MarshalManagedToNative (event_name));
          Marshal.WriteIntPtr (buffer, 4,
                               scheme_control.MarshalManagedToNative
                               (e.Button == MouseButtons.None       ? "None"
                                : e.Button == MouseButtons.Left     ? "Left"
                                : e.Button == MouseButtons.Right    ? "Right"
                                : e.Button == MouseButtons.XButton1 ? "XButton1"
                                : e.Button == MouseButtons.XButton2 ? "XButton2"
                                : "Multiple"));
          Marshal.WriteIntPtr (buffer, 8, scheme_control.MarshalManagedToNative (e.Clicks));
          Marshal.WriteIntPtr (buffer, 12, scheme_control.MarshalManagedToNative (e.Delta));
          Marshal.WriteIntPtr (buffer, 16, scheme_control.MarshalManagedToNative (e.X));
          Marshal.WriteIntPtr (buffer, 20, scheme_control.MarshalManagedToNative (e.Y));
          // Console.WriteLine ("Notify scheme of " + event_name);

          Scheme.scheme_apply (scheme_control, 6, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, System.ComponentModel.CancelEventArgs e)
    {
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (8);
          Marshal.WriteIntPtr (buffer, 0, scheme_control.MarshalManagedToNative (event_name));
          Marshal.WriteIntPtr (buffer, 4, scheme_control.MarshalManagedToNative (e.Cancel));
          // Console.WriteLine ("Notify scheme of " + event_name);

          Scheme.scheme_apply (scheme_control, 2, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, ControlEventArgs e)
    {
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (4);
          Marshal.WriteIntPtr (buffer, 0, scheme_control.MarshalManagedToNative (event_name));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_control, 1, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, DragEventArgs e)
    {
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (4 * 3);
          Marshal.WriteIntPtr (buffer, 0, scheme_control.MarshalManagedToNative (event_name));
          // other things go here
          Marshal.WriteIntPtr (buffer, 8, scheme_control.MarshalManagedToNative (e.X));
          Marshal.WriteIntPtr (buffer, 12, scheme_control.MarshalManagedToNative (e.Y));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_control, 3, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, GiveFeedbackEventArgs e)
    {
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (4);
          Marshal.WriteIntPtr (buffer, 0, scheme_control.MarshalManagedToNative (event_name));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_control, 1, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, HelpEventArgs e)
    {
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (4);
          Marshal.WriteIntPtr (buffer, 0, scheme_control.MarshalManagedToNative (event_name));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_control, 1, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, InvalidateEventArgs e)
    {
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (4);
          Marshal.WriteIntPtr (buffer, 0, scheme_control.MarshalManagedToNative (event_name));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_control, 1, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, InputLanguageChangingEventArgs e)
    {
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (4);
          Marshal.WriteIntPtr (buffer, 0, scheme_control.MarshalManagedToNative (event_name));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_control, 1, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, InputLanguageChangedEventArgs e)
    {
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (4);
          Marshal.WriteIntPtr (buffer, 0, scheme_control.MarshalManagedToNative (event_name));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_control, 1, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, KeyEventArgs e)
    {
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (4);
          Marshal.WriteIntPtr (buffer, 0, scheme_control.MarshalManagedToNative (event_name));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_control, 1, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, KeyPressEventArgs e)
    {
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (4);
          Marshal.WriteIntPtr (buffer, 0, scheme_control.MarshalManagedToNative (event_name));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_control, 1, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, LayoutEventArgs e)
    {
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (4);
          Marshal.WriteIntPtr (buffer, 0, scheme_control.MarshalManagedToNative (event_name));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_control, 1, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, QueryAccessibilityHelpEventArgs e)
    {
      IntPtr buffer = new IntPtr ();
      try {
          buffer = Marshal.AllocCoTaskMem (4 * 4);
          Marshal.WriteIntPtr (buffer, 0, scheme_control.MarshalManagedToNative (event_name));
          Marshal.WriteIntPtr (buffer, 4, scheme_control.MarshalManagedToNative (e.HelpKeyword));
          Marshal.WriteIntPtr (buffer, 8, scheme_control.MarshalManagedToNative (e.HelpNamespace));
          Marshal.WriteIntPtr (buffer, 12, scheme_control.MarshalManagedToNative (e.HelpString));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_control, 4, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, QueryContinueDragEventArgs e)
    {
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (4);
          Marshal.WriteIntPtr (buffer, 0, scheme_control.MarshalManagedToNative (event_name));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_control, 1, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, Message e)
    {
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (4);
          Marshal.WriteIntPtr (buffer, 0, scheme_control.MarshalManagedToNative (event_name));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_control, 1, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, PaintEventArgs e)
    {
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (4);
          Marshal.WriteIntPtr (buffer, 0, scheme_control.MarshalManagedToNative (event_name));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_control, 1, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    protected override void OnBackColorChanged (EventArgs e)
    {
      base.OnBackColorChanged (e);
      this.NotifyScheme ("BackColorChanged", e);
    }

    protected override void OnBackgroundImageChanged (EventArgs e)
    {
      base.OnBackgroundImageChanged (e);
      this.NotifyScheme ("BackgroundImageChanged", e);
    }

    protected override void OnBindingContextChanged (EventArgs e)
    {
      base.OnBindingContextChanged (e);
      this.NotifyScheme ("BindingContextChanged", e);
    }

    protected override void OnCausesValidationChanged (EventArgs e)
    {
      base.OnCausesValidationChanged (e);
      this.NotifyScheme ("CausesValidationChanged", e);
    }

    protected override void OnChangeUICues (UICuesEventArgs e)
    {
      base.OnChangeUICues (e);
      this.NotifyScheme ("ChangeUICues", e);
    }

    protected override void OnClick (EventArgs e)
    {
      base.OnClick (e);
      this.NotifyScheme ("Click", e);
    }

    protected override void OnContextMenuChanged (EventArgs e)
    {
      base.OnContextMenuChanged (e);
      this.NotifyScheme ("ContextMenuChanged", e);
    }

    protected override void OnControlAdded (ControlEventArgs e)
    {
      base.OnControlAdded (e);
      this.NotifyScheme ("ControlAdded", e);
    }

    protected override void OnControlRemoved (ControlEventArgs e)
    {
      base.OnControlRemoved (e);
      this.NotifyScheme ("ControlRemoved", e);
    }

    protected override void OnCursorChanged (EventArgs e)
    {
      base.OnCursorChanged (e);
      this.NotifyScheme ("CursorChanged", e);
    }

    protected override void OnDockChanged (EventArgs e)
    {
      base.OnDockChanged (e);
      this.NotifyScheme ("DockChanged", e);
    }

    protected override void OnDoubleClick (EventArgs e)
    {
      base.OnDoubleClick (e);
      this.NotifyScheme ("DoubleClick", e);
    }

    protected override void OnDragDrop (DragEventArgs e)
    {
      base.OnDragDrop (e);
      this.NotifyScheme ("DragDrop", e);
    }

    protected override void OnDragEnter (DragEventArgs e)
    {
      base.OnDragEnter (e);
      this.NotifyScheme ("DragEnter", e);
    }

    protected override void OnDragLeave (EventArgs e)
    {
      base.OnDragLeave (e);
      this.NotifyScheme ("DragLeave", e);
    }

    protected override void OnDragOver (DragEventArgs e)
    {
      base.OnDragOver (e);
      this.NotifyScheme ("DragOver", e);
    }

    protected override void OnEnabledChanged (EventArgs e)
    {
      base.OnEnabledChanged (e);
      this.NotifyScheme ("EnabledChanged", e);
    }

    protected override void OnEnter (EventArgs e)
    {
      base.OnEnter (e);
      this.NotifyScheme ("Enter", e);
    }

    protected override void OnFontChanged (EventArgs e)
    {
      base.OnFontChanged (e);
      this.NotifyScheme ("FontChanged", e);
    }

    protected override void OnForeColorChanged (EventArgs e)
    {
      base.OnForeColorChanged (e);
      this.NotifyScheme ("ForeColorChanged", e);
    }

    protected override void OnGiveFeedback (GiveFeedbackEventArgs e)
    {
      base.OnGiveFeedback (e);
      this.NotifyScheme ("GiveFeedback", e);
    }

    protected override void OnGotFocus (EventArgs e)
    {
      base.OnGotFocus (e);
      this.NotifyScheme ("GotFocus", e);
    }

    protected override void OnHandleCreated (EventArgs e)
    {
      base.OnHandleCreated (e);
      this.NotifyScheme ("HandleCreated", e);
    }

    protected override void OnHandleDestroyed (EventArgs e)
    {
      base.OnHandleDestroyed (e);
      this.NotifyScheme ("HandleDestroyed", e);
    }

    protected override void OnHelpRequested (HelpEventArgs e)
    {
      base.OnHelpRequested (e);
      this.NotifyScheme ("HelpRequested", e);
    }

    protected override void OnImeModeChanged (EventArgs e)
    {
      base.OnImeModeChanged (e);
      this.NotifyScheme ("ImeModeChanged", e);
    }

    protected override void OnInvalidated (InvalidateEventArgs e)
    {
      base.OnInvalidated (e);
      this.NotifyScheme ("Invalidated", e);
    }

    protected override void OnKeyDown (KeyEventArgs e)
    {
      base.OnKeyDown (e);
      this.NotifyScheme ("KeyDown", e);
    }

    protected override void OnKeyPress (KeyPressEventArgs e)
    {
      base.OnKeyPress (e);
      this.NotifyScheme ("KeyPress", e);
    }

    protected override void OnKeyUp (KeyEventArgs e)
    {
      base.OnKeyUp (e);
      this.NotifyScheme ("KeyUp", e);
    }

    protected override void OnLayout (LayoutEventArgs e)
    {
      base.OnLayout (e);
      this.NotifyScheme ("Layout", e);
    }

    protected override void OnLeave (EventArgs e)
    {
      base.OnLeave (e);
      this.NotifyScheme ("Leave", e);
    }

    protected override void OnLocationChanged (EventArgs e)
    {
      base.OnLocationChanged (e);
      this.NotifyScheme ("LocationChanged", e);
    }

    protected override void OnLostFocus (EventArgs e)
    {
      base.OnLostFocus (e);
      this.NotifyScheme ("LostFocus", e);
    }

    protected override void OnMouseDown (MouseEventArgs e)
    {
      base.OnMouseDown (e);
      this.NotifyScheme ("MouseDown", e);
    }

    protected override void OnMouseEnter (EventArgs e)
    {
      base.OnMouseEnter (e);
      this.NotifyScheme ("MouseEnter", e);
    }

    protected override void OnMouseHover (EventArgs e)
    {
      base.OnMouseHover (e);
      this.NotifyScheme ("MouseHover", e);
    }

    protected override void OnMouseMove (MouseEventArgs e)
    {
      base.OnMouseMove (e);
      this.NotifyScheme ("MouseMove", e);
    }

    protected override void OnMouseLeave (EventArgs e)
    {
      base.OnMouseLeave (e);
      this.NotifyScheme ("MouseLeave", e);
    }

    protected override void OnMouseUp (MouseEventArgs e)
    {
      base.OnMouseUp (e);
      this.NotifyScheme ("MouseUp", e);
    }

    protected override void OnMouseWheel (MouseEventArgs e)
    {
      base.OnMouseWheel (e);
      this.NotifyScheme ("MouseWheel", e);
    }

    protected override void OnMove (EventArgs e)
    {
      base.OnMove (e);
      this.NotifyScheme ("Move", e);
    }

    protected override void OnNotifyMessage (Message e)
    {
      base.OnNotifyMessage (e);
      this.NotifyScheme ("NotifyMessage", e);
    }

    protected override void OnPaint (PaintEventArgs e)
    {
      base.OnPaint (e);
      this.NotifyScheme ("Paint", e);
    }

    protected override void OnPaintBackground (PaintEventArgs e)
    {
      base.OnPaintBackground (e);
      this.NotifyScheme ("PaintBackground", e);
    }

    protected override void OnParentBackColorChanged (EventArgs e)
    {
      base.OnParentBackColorChanged (e);
      this.NotifyScheme ("ParentBackColorChanged", e);
    }

    protected override void OnParentBackgroundImageChanged (EventArgs e)
    {
      base.OnParentBackgroundImageChanged (e);
      this.NotifyScheme ("ParentBackgroundImageChanged", e);
    }

    protected override void OnParentBindingContextChanged (EventArgs e)
    {
      base.OnParentBindingContextChanged (e);
      this.NotifyScheme ("ParentBindingContextChanged", e);
    }

    protected override void OnParentChanged (EventArgs e)
    {
      base.OnParentChanged (e);
      this.NotifyScheme ("ParentChanged", e);
    }

    protected override void OnParentEnabledChanged (EventArgs e)
    {
      base.OnParentEnabledChanged (e);
      this.NotifyScheme ("ParentEnabledChanged", e);
    }

    protected override void OnParentFontChanged (EventArgs e)
    {
      base.OnParentFontChanged (e);
      this.NotifyScheme ("ParentFontChanged", e);
    }

    protected override void OnParentForeColorChanged (EventArgs e)
    {
      base.OnParentForeColorChanged (e);
      this.NotifyScheme ("ParentForeColorChanged", e);
    }

    protected override void OnParentRightToLeftChanged (EventArgs e)
    {
      base.OnParentRightToLeftChanged (e);
      this.NotifyScheme ("ParentRightToLeftChanged", e);
    }

    protected override void OnParentVisibleChanged (EventArgs e)
    {
      base.OnParentVisibleChanged (e);
      this.NotifyScheme ("ParentVisibleChanged", e);
    }

    protected override void OnQueryContinueDrag (QueryContinueDragEventArgs e)
    {
      base.OnQueryContinueDrag (e);
      this.NotifyScheme ("QueryContinueDrag", e);
    }

    protected override void OnResize (EventArgs e)
    {
      base.OnResize (e);
      this.NotifyScheme ("Resize", e);
    }

    protected override void OnRightToLeftChanged (EventArgs e)
    {
      base.OnRightToLeftChanged (e);
      this.NotifyScheme ("RightToLeftChanged", e);
    }

    protected override void OnSizeChanged (EventArgs e)
    {
      base.OnSizeChanged (e);
      this.NotifyScheme ("SizeChanged", e);
    }

    protected override void OnStyleChanged (EventArgs e)
    {
      base.OnStyleChanged (e);
      this.NotifyScheme ("StyleChanged", e);
    }

    protected override void OnSystemColorsChanged (EventArgs e)
    {
      base.OnSystemColorsChanged (e);
      this.NotifyScheme ("SystemColorsChanged", e);
    }

    protected override void OnTabIndexChanged (EventArgs e)
    {
      base.OnTabIndexChanged (e);
      this.NotifyScheme ("TabIndexChanged", e);
    }

    protected override void OnTabStopChanged (EventArgs e)
    {
      base.OnTabStopChanged (e);
      this.NotifyScheme ("TabStopChanged", e);
    }

    protected override void OnTextChanged (EventArgs e)
    {
      base.OnTextChanged (e);
      this.NotifyScheme ("TextChanged", e);
    }

    protected override void OnValidated (EventArgs e)
    {
      base.OnValidated (e);
      this.NotifyScheme ("Validated", e);
    }

    protected override void OnValidating (System.ComponentModel.CancelEventArgs e)
    {
      base.OnValidating (e);
      this.NotifyScheme ("Validating", e);
    }

    protected override void OnVisibleChanged (EventArgs e)
    {
      base.OnVisibleChanged (e);
      this.NotifyScheme ("VisibleChanged", e);
    }
  }


  public unsafe class ContainerControlProxy : System.Windows.Forms.ContainerControl
  {
    public MzSchemeObject scheme_form;

    public ContainerControlProxy (MzSchemeObject _scheme_form)
    {
      scheme_form = _scheme_form;
    }

    public new System.Windows.Forms.Control Parent
    {
       get {
           return base.Parent;
           }
       set {
           base.Parent = value;
           }
    }

    private void NotifyScheme (String event_name)
    {
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (4);
          Marshal.WriteIntPtr (buffer, 0, scheme_form.MarshalManagedToNative (event_name));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_form, 1, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, EventArgs e)
    {
      NotifyScheme (event_name);
    }

    private void NotifyScheme (String event_name, MouseEventArgs e)
    {
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (4 * 6);
          Marshal.WriteIntPtr (buffer, 0, scheme_form.MarshalManagedToNative (event_name));
          Marshal.WriteIntPtr (buffer, 4,
                               scheme_form.MarshalManagedToNative
                               (e.Button == MouseButtons.None       ? "None"
                                : e.Button == MouseButtons.Left     ? "Left"
                                : e.Button == MouseButtons.Right    ? "Right"
                                : e.Button == MouseButtons.XButton1 ? "XButton1"
                                : e.Button == MouseButtons.XButton2 ? "XButton2"
                                : "Multiple"));
          Marshal.WriteIntPtr (buffer, 8, scheme_form.MarshalManagedToNative (e.Clicks));
          Marshal.WriteIntPtr (buffer, 12, scheme_form.MarshalManagedToNative (e.Delta));
          Marshal.WriteIntPtr (buffer, 16, scheme_form.MarshalManagedToNative (e.X));
          Marshal.WriteIntPtr (buffer, 20, scheme_form.MarshalManagedToNative (e.Y));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_form, 6, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, System.ComponentModel.CancelEventArgs e)
    {
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (8);
          Marshal.WriteIntPtr (buffer, 0, scheme_form.MarshalManagedToNative (event_name));
          Marshal.WriteIntPtr (buffer, 4, scheme_form.MarshalManagedToNative (e.Cancel));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_form, 2, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, ControlEventArgs e)
    {
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (4);
          Marshal.WriteIntPtr (buffer, 0, scheme_form.MarshalManagedToNative (event_name));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_form, 1, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, DragEventArgs e)
    {
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (4 * 3);
          Marshal.WriteIntPtr (buffer, 0, scheme_form.MarshalManagedToNative (event_name));
          // other things go here
          Marshal.WriteIntPtr (buffer, 4, scheme_form.MarshalManagedToNative (e.X));
          Marshal.WriteIntPtr (buffer, 8, scheme_form.MarshalManagedToNative (e.Y));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_form, 3, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, GiveFeedbackEventArgs e)
    {
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (4);
          Marshal.WriteIntPtr (buffer, 0, scheme_form.MarshalManagedToNative (event_name));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_form, 1, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, HelpEventArgs e)
    {
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (4);
          Marshal.WriteIntPtr (buffer, 0, scheme_form.MarshalManagedToNative (event_name));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_form, 1, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, InvalidateEventArgs e)
    {
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (4);
          Marshal.WriteIntPtr (buffer, 0, scheme_form.MarshalManagedToNative (event_name));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_form, 1, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, InputLanguageChangingEventArgs e)
    {
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (4);
          Marshal.WriteIntPtr (buffer, 0, scheme_form.MarshalManagedToNative (event_name));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_form, 1, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, InputLanguageChangedEventArgs e)
    {
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (4);
          Marshal.WriteIntPtr (buffer, 0, scheme_form.MarshalManagedToNative (event_name));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_form, 1, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, KeyEventArgs e)
    {
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (4);
          Marshal.WriteIntPtr (buffer, 0, scheme_form.MarshalManagedToNative (event_name));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_form, 1, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, KeyPressEventArgs e)
    {
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (4);
          Marshal.WriteIntPtr (buffer, 0, scheme_form.MarshalManagedToNative (event_name));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_form, 1, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, LayoutEventArgs e)
    {
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (4);
          Marshal.WriteIntPtr (buffer, 0, scheme_form.MarshalManagedToNative (event_name));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_form, 1, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, QueryContinueDragEventArgs e)
    {
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (4);
          Marshal.WriteIntPtr (buffer, 0, scheme_form.MarshalManagedToNative (event_name));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_form, 1, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, Message e)
    {
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (4);
          Marshal.WriteIntPtr (buffer, 0, scheme_form.MarshalManagedToNative (event_name));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_form, 1, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, PaintEventArgs e)
    {
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (4);
          Marshal.WriteIntPtr (buffer, 0, scheme_form.MarshalManagedToNative (event_name));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_form, 1, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    protected override void OnBackColorChanged (EventArgs e)
    {
      base.OnBackColorChanged (e);
      this.NotifyScheme ("BackColorChanged", e);
    }

    protected override void OnBackgroundImageChanged (EventArgs e)
    {
      base.OnBackgroundImageChanged (e);
      this.NotifyScheme ("BackgroundImageChanged", e);
    }

    protected override void OnBindingContextChanged (EventArgs e)
    {
      base.OnBindingContextChanged (e);
      this.NotifyScheme ("BindingContextChanged", e);
    }

    protected override void OnCausesValidationChanged (EventArgs e)
    {
      base.OnCausesValidationChanged (e);
      this.NotifyScheme ("CausesValidationChanged", e);
    }

    protected override void OnChangeUICues (UICuesEventArgs e)
    {
      base.OnChangeUICues (e);
      this.NotifyScheme ("ChangeUICues", e);
    }

    protected override void OnClick (EventArgs e)
    {
      base.OnClick (e);
      this.NotifyScheme ("Click", e);
    }

    protected override void OnContextMenuChanged (EventArgs e)
    {
      base.OnContextMenuChanged (e);
      this.NotifyScheme ("ContextMenuChanged", e);
    }

    protected override void OnControlAdded (ControlEventArgs e)
    {
      base.OnControlAdded (e);
      this.NotifyScheme ("ControlAdded", e);
    }

    protected override void OnControlRemoved (ControlEventArgs e)
    {
      base.OnControlRemoved (e);
      this.NotifyScheme ("ControlRemoved", e);
    }

    protected override void OnCreateControl ()
    {
      base.OnCreateControl ();
      this.NotifyScheme ("CreateControl");
    }

    protected override void OnCursorChanged (EventArgs e)
    {
      base.OnCursorChanged (e);
      this.NotifyScheme ("CursorChanged", e);
    }

    protected override void OnDockChanged (EventArgs e)
    {
      base.OnDockChanged (e);
      this.NotifyScheme ("DockChanged", e);
    }

    protected override void OnDoubleClick (EventArgs e)
    {
      base.OnDoubleClick (e);
      this.NotifyScheme ("DoubleClick", e);
    }

    protected override void OnDragDrop (DragEventArgs e)
    {
      base.OnDragDrop (e);
      this.NotifyScheme ("DragDrop", e);
    }

    protected override void OnDragEnter (DragEventArgs e)
    {
      base.OnDragEnter (e);
      this.NotifyScheme ("DragEnter", e);
    }

    protected override void OnDragLeave (EventArgs e)
    {
      base.OnDragLeave (e);
      this.NotifyScheme ("DragLeave", e);
    }

    protected override void OnDragOver (DragEventArgs e)
    {
      base.OnDragOver (e);
      this.NotifyScheme ("DragOver", e);
    }

    protected override void OnEnabledChanged (EventArgs e)
    {
      base.OnEnabledChanged (e);
      this.NotifyScheme ("EnabledChanged", e);
    }

    protected override void OnEnter (EventArgs e)
    {
      base.OnEnter (e);
      this.NotifyScheme ("Enter", e);
    }

    protected override void OnFontChanged (EventArgs e)
    {
      base.OnFontChanged (e);
      this.NotifyScheme ("FontChanged", e);
    }

    protected override void OnForeColorChanged (EventArgs e)
    {
      base.OnForeColorChanged (e);
      this.NotifyScheme ("ForeColorChanged", e);
    }

    protected override void OnGiveFeedback (GiveFeedbackEventArgs e)
    {
      base.OnGiveFeedback (e);
      this.NotifyScheme ("GiveFeedback", e);
    }

    protected override void OnGotFocus (EventArgs e)
    {
      base.OnGotFocus (e);
      this.NotifyScheme ("GotFocus", e);
    }

    protected override void OnHandleCreated (EventArgs e)
    {
      base.OnHandleCreated (e);
      this.NotifyScheme ("HandleCreated", e);
    }

    protected override void OnHandleDestroyed (EventArgs e)
    {
      base.OnHandleDestroyed (e);
      this.NotifyScheme ("HandleDestroyed", e);
    }

    protected override void OnHelpRequested (HelpEventArgs e)
    {
      base.OnHelpRequested (e);
      this.NotifyScheme ("HelpRequested", e);
    }

    protected override void OnImeModeChanged (EventArgs e)
    {
      base.OnImeModeChanged (e);
      this.NotifyScheme ("ImeModeChanged", e);
    }

    protected override void OnInvalidated (InvalidateEventArgs e)
    {
      base.OnInvalidated (e);
      this.NotifyScheme ("Invalidated", e);
    }

    protected override void OnKeyDown (KeyEventArgs e)
    {
      base.OnKeyDown (e);
      this.NotifyScheme ("KeyDown", e);
    }

    protected override void OnKeyPress (KeyPressEventArgs e)
    {
      base.OnKeyPress (e);
      this.NotifyScheme ("KeyPress", e);
    }

    protected override void OnKeyUp (KeyEventArgs e)
    {
      base.OnKeyUp (e);
      this.NotifyScheme ("KeyUp", e);
    }

    protected override void OnLayout (LayoutEventArgs e)
    {
      base.OnLayout (e);
      this.NotifyScheme ("Layout", e);
    }

    protected override void OnLeave (EventArgs e)
    {
      base.OnLeave (e);
      this.NotifyScheme ("Leave", e);
    }

    protected override void OnLocationChanged (EventArgs e)
    {
      base.OnLocationChanged (e);
      this.NotifyScheme ("LocationChanged", e);
    }

    protected override void OnLostFocus (EventArgs e)
    {
      base.OnLostFocus (e);
      this.NotifyScheme ("LostFocus", e);
    }

    protected override void OnMouseDown (MouseEventArgs e)
    {
      base.OnMouseDown (e);
      this.NotifyScheme ("MouseDown", e);
    }

    protected override void OnMouseEnter (EventArgs e)
    {
      base.OnMouseEnter (e);
      this.NotifyScheme ("MouseEnter", e);
    }

    protected override void OnMouseHover (EventArgs e)
    {
      base.OnMouseHover (e);
      this.NotifyScheme ("MouseHover", e);
    }

    protected override void OnMouseLeave (EventArgs e)
    {
      base.OnMouseLeave (e);
      this.NotifyScheme ("MouseLeave", e);
    }

    protected override void OnMouseMove (MouseEventArgs e)
    {
      base.OnMouseMove (e);
      this.NotifyScheme ("MouseMove", e);
    }

    protected override void OnMouseUp (MouseEventArgs e)
    {
      base.OnMouseUp (e);
      this.NotifyScheme ("MouseUp", e);
    }

    protected override void OnMouseWheel (MouseEventArgs e)
    {
      base.OnMouseWheel (e);
      this.NotifyScheme ("MouseWheel", e);
    }

    protected override void OnMove (EventArgs e)
    {
      base.OnMove (e);
      this.NotifyScheme ("Move", e);
    }

    protected override void OnNotifyMessage (Message e)
    {
      base.OnNotifyMessage (e);
      this.NotifyScheme ("NotifyMessage", e);
    }

    protected override void OnPaint (PaintEventArgs e)
    {
      base.OnPaint (e);
      this.NotifyScheme ("Paint", e);
    }

    protected override void OnPaintBackground (PaintEventArgs e)
    {
      base.OnPaintBackground (e);
      this.NotifyScheme ("PaintBackground", e);
    }

    protected override void OnParentBackColorChanged (EventArgs e)
    {
      base.OnParentBackColorChanged (e);
      this.NotifyScheme ("ParentBackColorChanged", e);
    }

    protected override void OnParentBackgroundImageChanged (EventArgs e)
    {
      base.OnParentBackgroundImageChanged (e);
      this.NotifyScheme ("ParentBackgroundImageChanged", e);
    }

    protected override void OnParentBindingContextChanged (EventArgs e)
    {
      base.OnParentBindingContextChanged (e);
      this.NotifyScheme ("ParentBindingContextChanged", e);
    }

    protected override void OnParentChanged (EventArgs e)
    {
      base.OnParentChanged (e);
      this.NotifyScheme ("ParentChanged", e);
    }

    protected override void OnParentEnabledChanged (EventArgs e)
    {
      base.OnParentEnabledChanged (e);
      this.NotifyScheme ("ParentEnabledChanged", e);
    }

    protected override void OnParentFontChanged (EventArgs e)
    {
      base.OnParentFontChanged (e);
      this.NotifyScheme ("ParentFontChanged", e);
    }

    protected override void OnParentForeColorChanged (EventArgs e)
    {
      base.OnParentForeColorChanged (e);
      this.NotifyScheme ("ParentForeColorChanged", e);
    }

    protected override void OnParentRightToLeftChanged (EventArgs e)
    {
      base.OnParentRightToLeftChanged (e);
      this.NotifyScheme ("ParentRightToLeftChanged", e);
    }

    protected override void OnParentVisibleChanged (EventArgs e)
    {
      base.OnParentVisibleChanged (e);
      this.NotifyScheme ("ParentVisibleChanged", e);
    }

    protected override void OnQueryContinueDrag (QueryContinueDragEventArgs e)
    {
      base.OnQueryContinueDrag (e);
      this.NotifyScheme ("QueryContinueDrag", e);
    }

    protected override void OnResize (EventArgs e)
    {
      base.OnResize (e);
      this.NotifyScheme ("Resize", e);
    }

    protected override void OnRightToLeftChanged (EventArgs e)
    {
      base.OnRightToLeftChanged (e);
      this.NotifyScheme ("RightToLeftChanged", e);
    }

    protected override void OnSizeChanged (EventArgs e)
    {
      base.OnSizeChanged (e);
      this.NotifyScheme ("SizeChanged", e);
    }

    protected override void OnStyleChanged (EventArgs e)
    {
      base.OnStyleChanged (e);
      this.NotifyScheme ("StyleChanged", e);
    }

    protected override void OnSystemColorsChanged (EventArgs e)
    {
      base.OnSystemColorsChanged (e);
      this.NotifyScheme ("SystemColorsChanged", e);
    }

    protected override void OnTabIndexChanged (EventArgs e)
    {
      base.OnTabIndexChanged (e);
      this.NotifyScheme ("TabIndexChanged", e);
    }

    protected override void OnTabStopChanged (EventArgs e)
    {
      base.OnTabStopChanged (e);
      this.NotifyScheme ("TabStopChanged", e);
    }

    protected override void OnTextChanged (EventArgs e)
    {
      base.OnTextChanged (e);
      this.NotifyScheme ("TextChanged", e);
    }

    protected override void OnValidated (EventArgs e)
    {
      base.OnValidated (e);
      this.NotifyScheme ("Validated", e);
    }

    protected override void OnValidating (System.ComponentModel.CancelEventArgs e)
    {
      base.OnValidating (e);
      this.NotifyScheme ("Validating", e);
    }

    protected override void OnVisibleChanged (EventArgs e)
    {
      base.OnVisibleChanged (e);
      this.NotifyScheme ("VisibleChanged", e);
    }
  }

  public unsafe class FormProxy : System.Windows.Forms.Form
  {
    public MzSchemeObject scheme_form;

    public FormProxy (MzSchemeObject _scheme_form)
    {
      scheme_form = _scheme_form;
    }

    private void NotifyScheme (String event_name)
    {
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (4);
          Marshal.WriteIntPtr (buffer, 0, scheme_form.MarshalManagedToNative (event_name));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_form, 1, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, EventArgs e)
    {
      NotifyScheme (event_name);
    }

    private void NotifyScheme (String event_name, MouseEventArgs e)
    {
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (4 * 6);
          Marshal.WriteIntPtr (buffer, 0, scheme_form.MarshalManagedToNative (event_name));
          Marshal.WriteIntPtr (buffer, 4,
                               scheme_form.MarshalManagedToNative
                               (e.Button == MouseButtons.None       ? "None"
                                : e.Button == MouseButtons.Left     ? "Left"
                                : e.Button == MouseButtons.Right    ? "Right"
                                : e.Button == MouseButtons.XButton1 ? "XButton1"
                                : e.Button == MouseButtons.XButton2 ? "XButton2"
                                : "Multiple"));
          Marshal.WriteIntPtr (buffer, 8, scheme_form.MarshalManagedToNative (e.Clicks));
          Marshal.WriteIntPtr (buffer, 12, scheme_form.MarshalManagedToNative (e.Delta));
          Marshal.WriteIntPtr (buffer, 16, scheme_form.MarshalManagedToNative (e.X));
          Marshal.WriteIntPtr (buffer, 20, scheme_form.MarshalManagedToNative (e.Y));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_form, 6, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, System.ComponentModel.CancelEventArgs e)
    {
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (8);
          Marshal.WriteIntPtr (buffer, 0, scheme_form.MarshalManagedToNative (event_name));
          Marshal.WriteIntPtr (buffer, 4, scheme_form.MarshalManagedToNative (e.Cancel));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_form, 2, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, ControlEventArgs e)
    {
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (4);
          Marshal.WriteIntPtr (buffer, 0, scheme_form.MarshalManagedToNative (event_name));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_form, 1, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, DragEventArgs e)
    {
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (4 * 3);
          Marshal.WriteIntPtr (buffer, 0, scheme_form.MarshalManagedToNative (event_name));
          // other things go here
          Marshal.WriteIntPtr (buffer, 4, scheme_form.MarshalManagedToNative (e.X));
          Marshal.WriteIntPtr (buffer, 8, scheme_form.MarshalManagedToNative (e.Y));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_form, 3, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, GiveFeedbackEventArgs e)
    {
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (4);
          Marshal.WriteIntPtr (buffer, 0, scheme_form.MarshalManagedToNative (event_name));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_form, 1, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, HelpEventArgs e)
    {
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (4);
          Marshal.WriteIntPtr (buffer, 0, scheme_form.MarshalManagedToNative (event_name));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_form, 1, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, InvalidateEventArgs e)
    {
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (4);
          Marshal.WriteIntPtr (buffer, 0, scheme_form.MarshalManagedToNative (event_name));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_form, 1, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, InputLanguageChangingEventArgs e)
    {
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (4);
          Marshal.WriteIntPtr (buffer, 0, scheme_form.MarshalManagedToNative (event_name));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_form, 1, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, InputLanguageChangedEventArgs e)
    {
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (4);
          Marshal.WriteIntPtr (buffer, 0, scheme_form.MarshalManagedToNative (event_name));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_form, 1, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, KeyEventArgs e)
    {
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (4);
          Marshal.WriteIntPtr (buffer, 0, scheme_form.MarshalManagedToNative (event_name));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_form, 1, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, KeyPressEventArgs e)
    {
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (4);
          Marshal.WriteIntPtr (buffer, 0, scheme_form.MarshalManagedToNative (event_name));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_form, 1, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, LayoutEventArgs e)
    {
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (4);
          Marshal.WriteIntPtr (buffer, 0, scheme_form.MarshalManagedToNative (event_name));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_form, 1, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, QueryContinueDragEventArgs e)
    {
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (4);
          Marshal.WriteIntPtr (buffer, 0, scheme_form.MarshalManagedToNative (event_name));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_form, 1, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, Message e)
    {
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (4);
          Marshal.WriteIntPtr (buffer, 0, scheme_form.MarshalManagedToNative (event_name));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_form, 1, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, PaintEventArgs e)
    {
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (4);
          Marshal.WriteIntPtr (buffer, 0, scheme_form.MarshalManagedToNative (event_name));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_form, 1, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    protected override void OnActivated (EventArgs e)
    {
      base.OnActivated (e);
      this.NotifyScheme ("Activated", e);
    }

    protected override void OnBackColorChanged (EventArgs e)
    {
      base.OnBackColorChanged (e);
      this.NotifyScheme ("BackColorChanged", e);
    }

    protected override void OnBackgroundImageChanged (EventArgs e)
    {
      base.OnBackgroundImageChanged (e);
      this.NotifyScheme ("BackgroundImageChanged", e);
    }

    protected override void OnBindingContextChanged (EventArgs e)
    {
      base.OnBindingContextChanged (e);
      this.NotifyScheme ("BindingContextChanged", e);
    }

    protected override void OnCausesValidationChanged (EventArgs e)
    {
      base.OnCausesValidationChanged (e);
      this.NotifyScheme ("CausesValidationChanged", e);
    }

    protected override void OnChangeUICues (UICuesEventArgs e)
    {
      base.OnChangeUICues (e);
      this.NotifyScheme ("ChangeUICues", e);
    }

    protected override void OnClick (EventArgs e)
    {
      base.OnClick (e);
      this.NotifyScheme ("Click", e);
    }

    protected override void OnClosed (EventArgs e)
    {
      base.OnClosed (e);
      this.NotifyScheme ("Closed", e);
    }

    protected override void OnClosing (System.ComponentModel.CancelEventArgs e)
    {
      base.OnClosing (e);
      this.NotifyScheme ("Closing", e);
    }

    protected override void OnContextMenuChanged (EventArgs e)
    {
      base.OnContextMenuChanged (e);
      this.NotifyScheme ("ContextMenuChanged", e);
    }

    protected override void OnControlAdded (ControlEventArgs e)
    {
      base.OnControlAdded (e);
      this.NotifyScheme ("ControlAdded", e);
    }

    protected override void OnControlRemoved (ControlEventArgs e)
    {
      base.OnControlRemoved (e);
      this.NotifyScheme ("ControlRemoved", e);
    }

    protected override void OnCreateControl ()
    {
      base.OnCreateControl ();
      this.NotifyScheme ("CreateControl");
    }

    protected override void OnCursorChanged (EventArgs e)
    {
      base.OnCursorChanged (e);
      this.NotifyScheme ("CursorChanged", e);
    }

    protected override void OnDeactivate (EventArgs e)
    {
      base.OnDeactivate (e);
      this.NotifyScheme ("Deactivate", e);
    }

    protected override void OnDockChanged (EventArgs e)
    {
      base.OnDockChanged (e);
      this.NotifyScheme ("DockChanged", e);
    }

    protected override void OnDoubleClick (EventArgs e)
    {
      base.OnDoubleClick (e);
      this.NotifyScheme ("DoubleClick", e);
    }

    protected override void OnDragDrop (DragEventArgs e)
    {
      base.OnDragDrop (e);
      this.NotifyScheme ("DragDrop", e);
    }

    protected override void OnDragEnter (DragEventArgs e)
    {
      base.OnDragEnter (e);
      this.NotifyScheme ("DragEnter", e);
    }

    protected override void OnDragLeave (EventArgs e)
    {
      base.OnDragLeave (e);
      this.NotifyScheme ("DragLeave", e);
    }

    protected override void OnDragOver (DragEventArgs e)
    {
      base.OnDragOver (e);
      this.NotifyScheme ("DragOver", e);
    }

    protected override void OnEnabledChanged (EventArgs e)
    {
      base.OnEnabledChanged (e);
      this.NotifyScheme ("EnabledChanged", e);
    }

    protected override void OnEnter (EventArgs e)
    {
      base.OnEnter (e);
      this.NotifyScheme ("Enter", e);
    }

    protected override void OnFontChanged (EventArgs e)
    {
      base.OnFontChanged (e);
      this.NotifyScheme ("FontChanged", e);
    }

    protected override void OnForeColorChanged (EventArgs e)
    {
      base.OnForeColorChanged (e);
      this.NotifyScheme ("ForeColorChanged", e);
    }

    protected override void OnGiveFeedback (GiveFeedbackEventArgs e)
    {
      base.OnGiveFeedback (e);
      this.NotifyScheme ("GiveFeedback", e);
    }

    protected override void OnGotFocus (EventArgs e)
    {
      base.OnGotFocus (e);
      this.NotifyScheme ("GotFocus", e);
    }

    protected override void OnHandleCreated (EventArgs e)
    {
      base.OnHandleCreated (e);
      this.NotifyScheme ("HandleCreated", e);
    }

    protected override void OnHandleDestroyed (EventArgs e)
    {
      base.OnHandleDestroyed (e);
      this.NotifyScheme ("HandleDestroyed", e);
    }

    protected override void OnHelpRequested (HelpEventArgs e)
    {
      base.OnHelpRequested (e);
      this.NotifyScheme ("HelpRequested", e);
    }

    protected override void OnImeModeChanged (EventArgs e)
    {
      base.OnImeModeChanged (e);
      this.NotifyScheme ("ImeModeChanged", e);
    }

    protected override void OnInputLanguageChanged (InputLanguageChangedEventArgs e)
    {
      base.OnInputLanguageChanged (e);
      this.NotifyScheme ("InputLanguageChanged", e);
    }

    protected override void OnInputLanguageChanging (InputLanguageChangingEventArgs e)
    {
      base.OnInputLanguageChanging (e);
      this.NotifyScheme ("InputLanguageChanging", e);
    }

    protected override void OnInvalidated (InvalidateEventArgs e)
    {
      base.OnInvalidated (e);
      this.NotifyScheme ("Invalidated", e);
    }

    protected override void OnKeyDown (KeyEventArgs e)
    {
      base.OnKeyDown (e);
      this.NotifyScheme ("KeyDown", e);
    }

    protected override void OnKeyPress (KeyPressEventArgs e)
    {
      base.OnKeyPress (e);
      this.NotifyScheme ("KeyPress", e);
    }

    protected override void OnKeyUp (KeyEventArgs e)
    {
      base.OnKeyUp (e);
      this.NotifyScheme ("KeyUp", e);
    }

    protected override void OnLayout (LayoutEventArgs e)
    {
      base.OnLayout (e);
      this.NotifyScheme ("Layout", e);
    }

    protected override void OnLeave (EventArgs e)
    {
      base.OnLeave (e);
      this.NotifyScheme ("Leave", e);
    }

    protected override void OnLoad (EventArgs e)
    {
      base.OnLoad (e);
      this.NotifyScheme ("Load", e);
    }

    protected override void OnLocationChanged (EventArgs e)
    {
      base.OnLocationChanged (e);
      this.NotifyScheme ("LocationChanged", e);
    }

    protected override void OnLostFocus (EventArgs e)
    {
      base.OnLostFocus (e);
      this.NotifyScheme ("LostFocus", e);
    }

    protected override void OnMaximizedBoundsChanged (EventArgs e)
    {
      base.OnMaximizedBoundsChanged (e);
      this.NotifyScheme ("MaximizedBoundsChanged", e);
    }

    protected override void OnMaximumSizeChanged (EventArgs e)
    {
      base.OnMaximumSizeChanged (e);
      this.NotifyScheme ("MaximumSizeChanged", e);
    }

    protected override void OnMdiChildActivate (EventArgs e)
    {
      base.OnMdiChildActivate (e);
      this.NotifyScheme ("MdiChildActivate", e);
    }

    protected override void OnMenuComplete (EventArgs e)
    {
      base.OnMenuComplete (e);
      this.NotifyScheme ("MenuComplete", e);
    }

    protected override void OnMenuStart (EventArgs e)
    {
      base.OnMenuStart (e);
      this.NotifyScheme ("MenuStart", e);
    }

    protected override void OnMinimumSizeChanged (EventArgs e)
    {
      base.OnMinimumSizeChanged (e);
      this.NotifyScheme ("MinimumSizeChanged", e);
    }

    protected override void OnMouseDown (MouseEventArgs e)
    {
      base.OnMouseDown (e);
      this.NotifyScheme ("MouseDown", e);
    }

    protected override void OnMouseEnter (EventArgs e)
    {
      base.OnMouseEnter (e);
      this.NotifyScheme ("MouseEnter", e);
    }

    protected override void OnMouseHover (EventArgs e)
    {
      base.OnMouseHover (e);
      this.NotifyScheme ("MouseHover", e);
    }

    protected override void OnMouseMove (MouseEventArgs e)
    {
      base.OnMouseMove (e);
      this.NotifyScheme ("MouseMove", e);
    }

    protected override void OnMouseLeave (EventArgs e)
    {
      base.OnMouseLeave (e);
      this.NotifyScheme ("MouseLeave", e);
    }

    protected override void OnMouseUp (MouseEventArgs e)
    {
      base.OnMouseUp (e);
      this.NotifyScheme ("MouseUp", e);
    }

    protected override void OnMouseWheel (MouseEventArgs e)
    {
      base.OnMouseWheel (e);
      this.NotifyScheme ("MouseWheel", e);
    }

    protected override void OnMove (EventArgs e)
    {
      base.OnMove (e);
      this.NotifyScheme ("Move", e);
    }

    protected override void OnNotifyMessage (Message e)
    {
      base.OnNotifyMessage (e);
      this.NotifyScheme ("NotifyMessage", e);
    }

    protected override void OnPaint (PaintEventArgs e)
    {
      base.OnPaint (e);
      this.NotifyScheme ("Paint", e);
    }

    protected override void OnPaintBackground (PaintEventArgs e)
    {
      base.OnPaintBackground (e);
      this.NotifyScheme ("PaintBackground", e);
    }

    protected override void OnParentBackColorChanged (EventArgs e)
    {
      base.OnParentBackColorChanged (e);
      this.NotifyScheme ("ParentBackColorChanged", e);
    }

    protected override void OnParentBackgroundImageChanged (EventArgs e)
    {
      base.OnParentBackgroundImageChanged (e);
      this.NotifyScheme ("ParentBackgroundImageChanged", e);
    }

    protected override void OnParentBindingContextChanged (EventArgs e)
    {
      base.OnParentBindingContextChanged (e);
      this.NotifyScheme ("ParentBindingContextChanged", e);
    }

    protected override void OnParentChanged (EventArgs e)
    {
      base.OnParentChanged (e);
      this.NotifyScheme ("ParentChanged", e);
    }

    protected override void OnParentEnabledChanged (EventArgs e)
    {
      base.OnParentEnabledChanged (e);
      this.NotifyScheme ("ParentEnabledChanged", e);
    }

    protected override void OnParentFontChanged (EventArgs e)
    {
      base.OnParentFontChanged (e);
      this.NotifyScheme ("ParentFontChanged", e);
    }

    protected override void OnParentForeColorChanged (EventArgs e)
    {
      base.OnParentForeColorChanged (e);
      this.NotifyScheme ("ParentForeColorChanged", e);
    }

    protected override void OnParentRightToLeftChanged (EventArgs e)
    {
      base.OnParentRightToLeftChanged (e);
      this.NotifyScheme ("ParentRightToLeftChanged", e);
    }

    protected override void OnParentVisibleChanged (EventArgs e)
    {
      base.OnParentVisibleChanged (e);
      this.NotifyScheme ("ParentVisibleChanged", e);
    }

    protected override void OnQueryContinueDrag (QueryContinueDragEventArgs e)
    {
      base.OnQueryContinueDrag (e);
      this.NotifyScheme ("QueryContinueDrag", e);
    }

    protected override void OnResize (EventArgs e)
    {
      base.OnResize (e);
      this.NotifyScheme ("Resize", e);
    }

    protected override void OnRightToLeftChanged (EventArgs e)
    {
      base.OnRightToLeftChanged (e);
      this.NotifyScheme ("RightToLeftChanged", e);
    }

    protected override void OnSizeChanged (EventArgs e)
    {
      base.OnSizeChanged (e);
      this.NotifyScheme ("SizeChanged", e);
    }

    protected override void OnStyleChanged (EventArgs e)
    {
      base.OnStyleChanged (e);
      this.NotifyScheme ("StyleChanged", e);
    }

    protected override void OnSystemColorsChanged (EventArgs e)
    {
      base.OnSystemColorsChanged (e);
      this.NotifyScheme ("SystemColorsChanged", e);
    }

    protected override void OnTabIndexChanged (EventArgs e)
    {
      base.OnTabIndexChanged (e);
      this.NotifyScheme ("TabIndexChanged", e);
    }

    protected override void OnTabStopChanged (EventArgs e)
    {
      base.OnTabStopChanged (e);
      this.NotifyScheme ("TabStopChanged", e);
    }

    protected override void OnTextChanged (EventArgs e)
    {
      base.OnTextChanged (e);
      this.NotifyScheme ("TextChanged", e);
    }

    protected override void OnValidated (EventArgs e)
    {
      base.OnValidated (e);
      this.NotifyScheme ("Validated", e);
    }

    protected override void OnValidating (System.ComponentModel.CancelEventArgs e)
    {
      base.OnValidating (e);
      this.NotifyScheme ("Validating", e);
    }

    protected override void OnVisibleChanged (EventArgs e)
    {
      base.OnVisibleChanged (e);
      this.NotifyScheme ("VisibleChanged", e);
    }
  }

  public unsafe class RichTextProxy : System.Windows.Forms.RichTextBox
  {
    public MzSchemeObject scheme_form;

    public RichTextProxy (MzSchemeObject _scheme_form)
    {
      Console.WriteLine ("Creating a rich text proxy with " + _scheme_form);
      scheme_form = _scheme_form;
    }

    private void NotifyScheme (String event_name)
    {
      if (scheme_form == null) {
          Console.WriteLine ("Ignoring event " + event_name + " because object is uninitialized.");
          return;
          }
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (4);
          Marshal.WriteIntPtr (buffer, 0, scheme_form.MarshalManagedToNative (event_name));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_form, 1, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, EventArgs e)
    {
      if (scheme_form == null) {
          Console.WriteLine ("Ignoring event " + event_name + " because object is uninitialized.");
          return;
          }
      NotifyScheme (event_name);
    }

    private void NotifyScheme (String event_name, MouseEventArgs e)
    {
      if (scheme_form == null) {
          Console.WriteLine ("Ignoring event " + event_name + " because object is uninitialized.");
          return;
          }
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (4 * 6);
          Marshal.WriteIntPtr (buffer, 0, scheme_form.MarshalManagedToNative (event_name));
          Marshal.WriteIntPtr (buffer, 4,
                               scheme_form.MarshalManagedToNative
                               (e.Button == MouseButtons.None       ? "None"
                                : e.Button == MouseButtons.Left     ? "Left"
                                : e.Button == MouseButtons.Right    ? "Right"
                                : e.Button == MouseButtons.XButton1 ? "XButton1"
                                : e.Button == MouseButtons.XButton2 ? "XButton2"
                                : "Multiple"));
          Marshal.WriteIntPtr (buffer, 8, scheme_form.MarshalManagedToNative (e.Clicks));
          Marshal.WriteIntPtr (buffer, 12, scheme_form.MarshalManagedToNative (e.Delta));
          Marshal.WriteIntPtr (buffer, 16, scheme_form.MarshalManagedToNative (e.X));
          Marshal.WriteIntPtr (buffer, 20, scheme_form.MarshalManagedToNative (e.Y));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_form, 6, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, System.ComponentModel.CancelEventArgs e)
    {
      if (scheme_form == null) {
          Console.WriteLine ("Ignoring event " + event_name + " because object is uninitialized.");
          return;
          }
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (8);
          Marshal.WriteIntPtr (buffer, 0, scheme_form.MarshalManagedToNative (event_name));
          Marshal.WriteIntPtr (buffer, 4, scheme_form.MarshalManagedToNative (e.Cancel));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_form, 2, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, ContentsResizedEventArgs e)
    {
      if (scheme_form == null) {
          Console.WriteLine ("Ignoring event " + event_name + " because object is uninitialized.");
          return;
          }
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (4);
          Marshal.WriteIntPtr (buffer, 0, scheme_form.MarshalManagedToNative (event_name));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_form, 1, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, ControlEventArgs e)
    {
      if (scheme_form == null) {
          Console.WriteLine ("Ignoring event " + event_name + " because object is uninitialized.");
          return;
          }
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (4);
          Marshal.WriteIntPtr (buffer, 0, scheme_form.MarshalManagedToNative (event_name));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_form, 1, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, DragEventArgs e)
    {
      if (scheme_form == null) {
          Console.WriteLine ("Ignoring event " + event_name + " because object is uninitialized.");
          return;
          }
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (4 * 3);
          Marshal.WriteIntPtr (buffer, 0, scheme_form.MarshalManagedToNative (event_name));
          // other things go here
          Marshal.WriteIntPtr (buffer, 4, scheme_form.MarshalManagedToNative (e.X));
          Marshal.WriteIntPtr (buffer, 8, scheme_form.MarshalManagedToNative (e.Y));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_form, 3, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, GiveFeedbackEventArgs e)
    {
      if (scheme_form == null) {
          Console.WriteLine ("Ignoring event " + event_name + " because object is uninitialized.");
          return;
          }
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (4);
          Marshal.WriteIntPtr (buffer, 0, scheme_form.MarshalManagedToNative (event_name));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_form, 1, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, HelpEventArgs e)
    {
      if (scheme_form == null) {
          Console.WriteLine ("Ignoring event " + event_name + " because object is uninitialized.");
          return;
          }
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (4);
          Marshal.WriteIntPtr (buffer, 0, scheme_form.MarshalManagedToNative (event_name));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_form, 1, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, InvalidateEventArgs e)
    {
      if (scheme_form == null) {
          Console.WriteLine ("Ignoring event " + event_name + " because object is uninitialized.");
          return;
          }
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (4);
          Marshal.WriteIntPtr (buffer, 0, scheme_form.MarshalManagedToNative (event_name));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_form, 1, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, InputLanguageChangingEventArgs e)
    {
      if (scheme_form == null) {
          Console.WriteLine ("Ignoring event " + event_name + " because object is uninitialized.");
          return;
          }
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (4);
          Marshal.WriteIntPtr (buffer, 0, scheme_form.MarshalManagedToNative (event_name));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_form, 1, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, InputLanguageChangedEventArgs e)
    {
      if (scheme_form == null) {
          Console.WriteLine ("Ignoring event " + event_name + " because object is uninitialized.");
          return;
          }
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (4);
          Marshal.WriteIntPtr (buffer, 0, scheme_form.MarshalManagedToNative (event_name));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_form, 1, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, KeyEventArgs e)
    {
      if (scheme_form == null) {
          Console.WriteLine ("Ignoring event " + event_name + " because object is uninitialized.");
          return;
          }
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (4);
          Marshal.WriteIntPtr (buffer, 0, scheme_form.MarshalManagedToNative (event_name));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_form, 1, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, KeyPressEventArgs e)
    {
      if (scheme_form == null) {
          Console.WriteLine ("Ignoring event " + event_name + " because object is uninitialized.");
          return;
          }
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (4);
          Marshal.WriteIntPtr (buffer, 0, scheme_form.MarshalManagedToNative (event_name));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_form, 1, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, LayoutEventArgs e)
    {
      if (scheme_form == null) {
          Console.WriteLine ("Ignoring event " + event_name + " because object is uninitialized.");
          return;
          }
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (4);
          Marshal.WriteIntPtr (buffer, 0, scheme_form.MarshalManagedToNative (event_name));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_form, 1, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, LinkClickedEventArgs e)
    {
      if (scheme_form == null) {
          Console.WriteLine ("Ignoring event " + event_name + " because object is uninitialized.");
          return;
          }
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (8);
          Marshal.WriteIntPtr (buffer, 0, scheme_form.MarshalManagedToNative (event_name));
          Marshal.WriteIntPtr (buffer, 4, scheme_form.MarshalManagedToNative (e.LinkText));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_form, 2, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, QueryContinueDragEventArgs e)
    {
      if (scheme_form == null) {
          Console.WriteLine ("Ignoring event " + event_name + " because object is uninitialized.");
          return;
          }
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (4);
          Marshal.WriteIntPtr (buffer, 0, scheme_form.MarshalManagedToNative (event_name));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_form, 1, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, Message e)
    {
      if (scheme_form == null) {
          Console.WriteLine ("Ignoring event " + event_name + " because object is uninitialized.");
          return;
          }
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (4);
          Marshal.WriteIntPtr (buffer, 0, scheme_form.MarshalManagedToNative (event_name));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_form, 1, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    private void NotifyScheme (String event_name, PaintEventArgs e)
    {
      if (scheme_form == null) {
          Console.WriteLine ("Ignoring event " + event_name + " because object is uninitialized.");
          return;
          }
      IntPtr buffer = new IntPtr();
      try {
          buffer = Marshal.AllocCoTaskMem (4);
          Marshal.WriteIntPtr (buffer, 0, scheme_form.MarshalManagedToNative (event_name));
          // Console.WriteLine ("Notify scheme of " + event_name);
          Scheme.scheme_apply (scheme_form, 1, buffer);
          }
      finally {
          if (!IntPtr.Zero.Equals (buffer)) Marshal.FreeCoTaskMem (buffer);
          }
    }

    protected override void OnBackColorChanged (EventArgs e)
    {
      base.OnBackColorChanged (e);
      this.NotifyScheme ("BackColorChanged", e);
    }

    protected override void OnBackgroundImageChanged (EventArgs e)
    {
      base.OnBackgroundImageChanged (e);
      this.NotifyScheme ("BackgroundImageChanged", e);
    }

    protected override void OnBindingContextChanged (EventArgs e)
    {
      base.OnBindingContextChanged (e);
      this.NotifyScheme ("BindingContextChanged", e);
    }

    protected override void OnBorderStyleChanged (EventArgs e)
    {
      base.OnBorderStyleChanged (e);
      this.NotifyScheme ("BorderStyleChanged", e);
    }

    protected override void OnCausesValidationChanged (EventArgs e)
    {
      base.OnCausesValidationChanged (e);
      this.NotifyScheme ("CausesValidationChanged", e);
    }

    protected override void OnChangeUICues (UICuesEventArgs e)
    {
      base.OnChangeUICues (e);
      this.NotifyScheme ("ChangeUICues", e);
    }

    protected override void OnClick (EventArgs e)
    {
      base.OnClick (e);
      this.NotifyScheme ("Click", e);
    }

    protected override void OnContentsResized (ContentsResizedEventArgs e)
    {
      base.OnContentsResized(e);
      this.NotifyScheme ("ContentsResized", e);
    }

    protected override void OnContextMenuChanged (EventArgs e)
    {
      base.OnContextMenuChanged (e);
      this.NotifyScheme ("ContextMenuChanged", e);
    }

    protected override void OnControlAdded (ControlEventArgs e)
    {
      base.OnControlAdded (e);
      this.NotifyScheme ("ControlAdded", e);
    }

    protected override void OnControlRemoved (ControlEventArgs e)
    {
      base.OnControlRemoved (e);
      this.NotifyScheme ("ControlRemoved", e);
    }

    protected override void OnCreateControl ()
    {
      base.OnCreateControl ();
      this.NotifyScheme ("CreateControl");
    }

    protected override void OnCursorChanged (EventArgs e)
    {
      base.OnCursorChanged (e);
      this.NotifyScheme ("CursorChanged", e);
    }

    protected override void OnDockChanged (EventArgs e)
    {
      base.OnDockChanged (e);
      this.NotifyScheme ("DockChanged", e);
    }

    protected override void OnDoubleClick (EventArgs e)
    {
      base.OnDoubleClick (e);
      this.NotifyScheme ("DoubleClick", e);
    }

    protected override void OnDragDrop (DragEventArgs e)
    {
      base.OnDragDrop (e);
      this.NotifyScheme ("DragDrop", e);
    }

    protected override void OnDragEnter (DragEventArgs e)
    {
      base.OnDragEnter (e);
      this.NotifyScheme ("DragEnter", e);
    }

    protected override void OnDragLeave (EventArgs e)
    {
      base.OnDragLeave (e);
      this.NotifyScheme ("DragLeave", e);
    }

    protected override void OnDragOver (DragEventArgs e)
    {
      base.OnDragOver (e);
      this.NotifyScheme ("DragOver", e);
    }

    protected override void OnEnabledChanged (EventArgs e)
    {
      base.OnEnabledChanged (e);
      this.NotifyScheme ("EnabledChanged", e);
    }

    protected override void OnEnter (EventArgs e)
    {
      base.OnEnter (e);
      this.NotifyScheme ("Enter", e);
    }

    protected override void OnFontChanged (EventArgs e)
    {
      base.OnFontChanged (e);
      this.NotifyScheme ("FontChanged", e);
    }

    protected override void OnForeColorChanged (EventArgs e)
    {
      base.OnForeColorChanged (e);
      this.NotifyScheme ("ForeColorChanged", e);
    }

    protected override void OnGiveFeedback (GiveFeedbackEventArgs e)
    {
      base.OnGiveFeedback (e);
      this.NotifyScheme ("GiveFeedback", e);
    }

    protected override void OnGotFocus (EventArgs e)
    {
      base.OnGotFocus (e);
      this.NotifyScheme ("GotFocus", e);
    }

    protected override void OnHandleCreated (EventArgs e)
    {
      base.OnHandleCreated (e);
      this.NotifyScheme ("HandleCreated", e);
    }

    protected override void OnHandleDestroyed (EventArgs e)
    {
      base.OnHandleDestroyed (e);
      this.NotifyScheme ("HandleDestroyed", e);
    }

    protected override void OnHelpRequested (HelpEventArgs e)
    {
      base.OnHelpRequested (e);
      this.NotifyScheme ("HelpRequested", e);
    }

    protected override void OnHideSelectionChanged (EventArgs e)
    {
      base.OnHideSelectionChanged (e);
      this.NotifyScheme ("HideSelectionChanged", e);
    }

    protected override void OnHScroll (EventArgs e)
    {
      base.OnHScroll (e);
      this.NotifyScheme ("HScroll", e);
    }

    protected override void OnImeChange (EventArgs e)
    {
      base.OnImeModeChanged (e);
      this.NotifyScheme ("ImeChange", e);
    }

    protected override void OnImeModeChanged (EventArgs e)
    {
      base.OnImeModeChanged (e);
      this.NotifyScheme ("ImeModeChanged", e);
    }

    protected override void OnInvalidated (InvalidateEventArgs e)
    {
      base.OnInvalidated (e);
      this.NotifyScheme ("Invalidated", e);
    }

    protected override void OnKeyDown (KeyEventArgs e)
    {
      base.OnKeyDown (e);
      this.NotifyScheme ("KeyDown", e);
    }

    protected override void OnKeyPress (KeyPressEventArgs e)
    {
      base.OnKeyPress (e);
      this.NotifyScheme ("KeyPress", e);
    }

    protected override void OnKeyUp (KeyEventArgs e)
    {
      base.OnKeyUp (e);
      this.NotifyScheme ("KeyUp", e);
    }

    protected override void OnLayout (LayoutEventArgs e)
    {
      base.OnLayout (e);
      this.NotifyScheme ("Layout", e);
    }

    protected override void OnLeave (EventArgs e)
    {
      base.OnLeave (e);
      this.NotifyScheme ("Leave", e);
    }

    protected override void OnLinkClicked (LinkClickedEventArgs e)
    {
      base.OnLinkClicked (e);
      this.NotifyScheme ("LinkClicked", e);
    }

    protected override void OnLocationChanged (EventArgs e)
    {
      base.OnLocationChanged (e);
      this.NotifyScheme ("LocationChanged", e);
    }

    protected override void OnLostFocus (EventArgs e)
    {
      base.OnLostFocus (e);
      this.NotifyScheme ("LostFocus", e);
    }

    protected override void OnModifiedChanged (EventArgs e)
    {
      base.OnModifiedChanged (e);
      this.NotifyScheme ("ModifiedChanged", e);
    }

    protected override void OnMouseDown (MouseEventArgs e)
    {
      base.OnMouseDown (e);
      this.NotifyScheme ("MouseDown", e);
    }

    protected override void OnMouseEnter (EventArgs e)
    {
      base.OnMouseEnter (e);
      this.NotifyScheme ("MouseEnter", e);
    }

    protected override void OnMouseHover (EventArgs e)
    {
      base.OnMouseHover (e);
      this.NotifyScheme ("MouseHover", e);
    }

    protected override void OnMouseMove (MouseEventArgs e)
    {
      base.OnMouseMove (e);
      this.NotifyScheme ("MouseMove", e);
    }

    protected override void OnMouseLeave (EventArgs e)
    {
      base.OnMouseLeave (e);
      this.NotifyScheme ("MouseLeave", e);
    }

    protected override void OnMouseUp (MouseEventArgs e)
    {
      base.OnMouseUp (e);
      this.NotifyScheme ("MouseUp", e);
    }

    protected override void OnMouseWheel (MouseEventArgs e)
    {
      base.OnMouseWheel (e);
      this.NotifyScheme ("MouseWheel", e);
    }

    protected override void OnMove (EventArgs e)
    {
      base.OnMove (e);
      this.NotifyScheme ("Move", e);
    }

    protected override void OnMultilineChanged (EventArgs e)
    {
      base.OnMultilineChanged (e);
      this.NotifyScheme ("MultilineChanged", e);
    }

    protected override void OnNotifyMessage (Message e)
    {
      base.OnNotifyMessage (e);
      this.NotifyScheme ("NotifyMessage", e);
    }

    protected override void OnPaint (PaintEventArgs e)
    {
      base.OnPaint (e);
      this.NotifyScheme ("Paint", e);
    }

    protected override void OnPaintBackground (PaintEventArgs e)
    {
      base.OnPaintBackground (e);
      this.NotifyScheme ("PaintBackground", e);
    }

    protected override void OnParentBackColorChanged (EventArgs e)
    {
      base.OnParentBackColorChanged (e);
      this.NotifyScheme ("ParentBackColorChanged", e);
    }

    protected override void OnParentBackgroundImageChanged (EventArgs e)
    {
      base.OnParentBackgroundImageChanged (e);
      this.NotifyScheme ("ParentBackgroundImageChanged", e);
    }

    protected override void OnParentBindingContextChanged (EventArgs e)
    {
      base.OnParentBindingContextChanged (e);
      this.NotifyScheme ("ParentBindingContextChanged", e);
    }

    protected override void OnParentChanged (EventArgs e)
    {
      base.OnParentChanged (e);
      this.NotifyScheme ("ParentChanged", e);
    }

    protected override void OnParentEnabledChanged (EventArgs e)
    {
      base.OnParentEnabledChanged (e);
      this.NotifyScheme ("ParentEnabledChanged", e);
    }

    protected override void OnParentFontChanged (EventArgs e)
    {
      base.OnParentFontChanged (e);
      this.NotifyScheme ("ParentFontChanged", e);
    }

    protected override void OnParentForeColorChanged (EventArgs e)
    {
      base.OnParentForeColorChanged (e);
      this.NotifyScheme ("ParentForeColorChanged", e);
    }

    protected override void OnParentRightToLeftChanged (EventArgs e)
    {
      base.OnParentRightToLeftChanged (e);
      this.NotifyScheme ("ParentRightToLeftChanged", e);
    }

    protected override void OnParentVisibleChanged (EventArgs e)
    {
      base.OnParentVisibleChanged (e);
      this.NotifyScheme ("ParentVisibleChanged", e);
    }

    protected override void OnProtected (EventArgs e)
    {
      base.OnProtected (e);
      this.NotifyScheme ("Protected", e);
    }

    protected override void OnQueryContinueDrag (QueryContinueDragEventArgs e)
    {
      base.OnQueryContinueDrag (e);
      this.NotifyScheme ("QueryContinueDrag", e);
    }

    protected override void OnReadOnlyChanged (EventArgs e)
    {
      base.OnReadOnlyChanged (e);
      this.NotifyScheme ("ReadOnlyChanged", e);
    }

    protected override void OnResize (EventArgs e)
    {
      base.OnResize (e);
      this.NotifyScheme ("Resize", e);
    }

    protected override void OnRightToLeftChanged (EventArgs e)
    {
      base.OnRightToLeftChanged (e);
      this.NotifyScheme ("RightToLeftChanged", e);
    }

    protected override void OnSelectionChanged (EventArgs e)
    {
      base.OnSelectionChanged (e);
      this.NotifyScheme ("SelectionChanged", e);
    }

    protected override void OnSizeChanged (EventArgs e)
    {
      base.OnSizeChanged (e);
      this.NotifyScheme ("SizeChanged", e);
    }

    protected override void OnStyleChanged (EventArgs e)
    {
      base.OnStyleChanged (e);
      this.NotifyScheme ("StyleChanged", e);
    }

    protected override void OnSystemColorsChanged (EventArgs e)
    {
      base.OnSystemColorsChanged (e);
      this.NotifyScheme ("SystemColorsChanged", e);
    }

    protected override void OnTabIndexChanged (EventArgs e)
    {
      base.OnTabIndexChanged (e);
      this.NotifyScheme ("TabIndexChanged", e);
    }

    protected override void OnTabStopChanged (EventArgs e)
    {
      base.OnTabStopChanged (e);
      this.NotifyScheme ("TabStopChanged", e);
    }

    protected override void OnTextChanged (EventArgs e)
    {
      base.OnTextChanged (e);
      this.NotifyScheme ("TextChanged", e);
    }

    protected override void OnValidated (EventArgs e)
    {
      base.OnValidated (e);
      this.NotifyScheme ("Validated", e);
    }

    protected override void OnValidating (System.ComponentModel.CancelEventArgs e)
    {
      base.OnValidating (e);
      this.NotifyScheme ("Validating", e);
    }

    protected override void OnVisibleChanged (EventArgs e)
    {
      base.OnVisibleChanged (e);
      this.NotifyScheme ("VisibleChanged", e);
    }

    protected override void OnVScroll (EventArgs e)
    {
      base.OnVScroll (e);
      this.NotifyScheme ("VScroll", e);
    }
  }

  public class MessageFilter : System.Windows.Forms.IMessageFilter
  {
    public Helper helper;

    public MessageFilter (Helper _helper)
    {
      helper = _helper;
    }

    public virtual Boolean PreFilterMessage (ref System.Windows.Forms.Message msg)
    {
      helper.TriggerRandomEvent();
      return false;
    }
  }


  public class ApplicationContext : System.Windows.Forms.ApplicationContext
  {
    public Helper helper;

    public ApplicationContext (Helper _helper)
    {
      helper = _helper;
    }

    public static void main (Helper _helper)
    {
      System.Windows.Forms.Application.AddMessageFilter (new MessageFilter (_helper));
      Application.Run (new ApplicationContext (_helper));
      return;
    }
  }

  // This is here so we can create an executable
  // so we can execute this assembly
  // so we have an ExecutingAssembly
  // so we can push a message loop
  public class DummyMain
  {

    [STAThread]
    public static void Main ()
    {
      return;
    }
  }
}
