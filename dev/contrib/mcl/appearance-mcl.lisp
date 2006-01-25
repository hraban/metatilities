;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;; appearance-mcl.lisp
;;;
;;; Gives MCL a more modern look & feel in accordance with Appearance, Themes and (for OSX) Aqua.
;;; Includes improvements to MCL and to the Appearance Manager modules in the Examples folder of MCL.
;;;
;;; Version 1.5c1
;;; Changes to original MCL code Copyright © 1999-2004 Terje Norderhaug and in¥Progress.
;;; All source code from MCL is copyright © Digitool. Other source code is copyright their respective authors.
;;;
;;; Use and copying of this software and preparation of derivative works in MCL
;;; based upon this software are permitted, so long as this copyright 
;;; notice is included intact in the source code.
;;;
;;; Digitool is welcome and encouraged to integrate parts or whole of this module in MCL
;;; as long as the author is mentioned in their version history ;-)
;;;
;;; This software is made available AS IS, and no warranty is made about 
;;; the software or its performance.
;;;
;;; Author: Terje Norderhaug <terje@in-progress.com> of in¥Progress.
;;;
;;; Latest version available from <http://www.in-progress.com/src/>.
;;; Tested on MCL 4.3.1, MCL 4.3.5, 5.0 and 5.1b2, but should also work with other recent versions of MCL.
;;; Should usually be loaded after other patches and modules.
;;;
;;; Most modifications to original MCL source code are in color.

#| VERSION HISTORY
2004-Jun-04 Terje Changes for compatability with MCL 5.1b2:
                   The frame-table-item function has an added argument 'inset' in MCL 5.1.
                   Don't override view-draw-contents for arrow-dialog-item in MCL 5.1.
                   Removed set-dialog-item-text on static-text-dialog-item for MCL 5.1 as it is the same as in MCL.
                   Substituted (mouse-down-p) with (wait-mouse-up-or-moved) for MCL 5.1.
                   Initialize-instance :before on ed-help-window moves fred-title up one pixel in MCL 5.1.
2004-Jun-04 Terje Added view-pixel-depth and view-color-p functions and used them wherever required.
2004-Jun-04 Terje Uses theme text color as default for fred-dialog-item:
                   Eliminates view-draw-contents :around on basic-editable-text-dialog-item.
                   Added a patched view-draw-contents on fred-dialog-item that calls #_setThemetextColor.
2004-May-27 Terje Added an invalidate-view method on static-text-dialog-item under MCL 5 and beyond that provides T as default for erase-p.
2004-May-27 Terje Using a case construct to set the value of text-justification in view-draw-contents for static-text-dialog-item.
2004-May-23 Terje Theme compliant hilighting of table cells:
                   Modified %draw-table-cell-new to use a theme background for the hilight color instead of invert.
2004-May-09 Terje The label-offset function for title-box-dialog-item works even when the box has no title. 
2004-May-05 Terje view-activate-event-handler :before on pop-up-menu checks whether menu-enabled-p before activting the control.
2004-Apr-28 Terje Added patches for clock-dialog-items and the progress bar of the Appearance Manager.
2004-Apr-26 Terje theme-tab-title-proc uses #_GetThemeTextDimensions to get the font height.
2004-Apr-25 Terje Small fix to theme-tab-title-proc so the tab title is vertically centered in OSX.
2004-Apr-25 Terje The pop-up arrow of the pull-down menu is optional (suggested by Alice).
2004-Apr-25 Terje Poof button, bar dragger and backtrace's dragging pane splitter has legacy triangles icon for Alice.
2004-Apr-25 Terje The Fred pane splitter is black for Alice.
2004-Apr-25 Terje The feature :alice determines whether to adhere to Alice/Digitool's wishes for next gen MCL UI.
2004-Apr-20 Terje New view-setup-background for multi-pane-view to ensure proper background for contained controls.
2004-Apr-19 Terje New view-setup-background method to set up custom background for a control.
2004-Apr-19 Terje New view-apply-text-color to set up custom text color for a control.
2004-Apr-19 terje New handle->dialog-item to map from a control handle to its view.
2004-Apr-19 terje New control-color-proc callback to set custom text color and background for controls.
2004-Apr-15 terje Calling inset-rect changed to #_insetRect in theme-tab-title-proc.
2004-Apr-10 Terje Pane tabs are sized and drawn depending on theme text dimensions.
2004-Apr-10 Terje Subviews of Multi-pane-view uses the background of the pane instead of the one of the dialog:
                    Added call to applyThemeBackground in view-draw-contents for multi-pane-view.
2004-Apr-10 terje Minor changes to initialization of appearance manager
2004-Apr-08 Terje draw-up-rect fixed to work in MCL 4.3.1 when highlight-color is a number.
2004-Apr-07 Terje Version 1.4 released.
2004-Apr-01 Terje Several minor changes for better functionality under MCL 4.3.1.
2004-Mar-29 Terje view-draw-contents on pull-down-menu uses theme text colors.
                  Added inspector::set-selection to fix problems displaying selection in inspector under MacOS9.
                  New view-draw-contents for ellipsized-text-dialog-item in MCL 5.1.
                  Add MCL 5.0 modifications to view-(de)activate-event-handler for static-text-dialog-item.
                  Separator drawn below command pane in OSX.
2004-Mar-14 Terje Moves without-interrupts in view-click-event-handler for table-dialog-item as suggested in Digitool's woi-patch.
2004-Mar-14 Terje Pull down menus have nicer dimensions:
                   view-default-size on pull-down-menu uses GetThemeTextDimensions to get proper size for theme text.
                   Moved the menu text a few pixels to the right by changing view-draw-contents and view-click-event-handler.
2004-Feb-18 Terje Pull down menu more theme savvy:
                   New view-draw-contents for pull-down-menu to draw theme arrow etc.
                   Added view-click-event-handler for pull-down-menu that draws theme text and background when the menu is selected.
2004-Feb-14 Terje The Backtrace, Processes and Inspectors are now almost completely theme savvy.
2004-Feb-14 Terje Theme fonts for backtrace command and -info panes (suggested by Octav Popescu):
                   Added add-command-pane-items on command-pane with theme fonts.
                   Added initialize-instance on backtrace-info-pane with theme fonts.
                   Added initialize-instance on backtrace-command-pane with theme fonts.
                   New view-draw-contents on backtrace-info-pane to draw placard.
2004-Feb-10 Terje Improved appearance for the Bar Dragger under OSX (rubber instead of arrow).
2004-Feb-10 Terje Improved appearance for the pane splitter under OSX.
2004-Feb-03 Terje Adjusted the Bar Dragger for OSX so it is aligned with other arrows:
                   Added patch to draw-vertical-dragger.
2004-Feb-02 terje Modern 3D buttons to illustrate special keys in the Fred Commands window (and the Listener Commands window):
                   The key-cap class has 3d-button as superclass.
                   view-draw-contents on key-cap calls next method instead of drawing anything.
                   pushed-state and dialog-item-text methods on key-cap.
2004-feb-02 Terje The Inspector and BackTrace dialogs have more of a theme appearance:
                   Added a view-draw-contents :around for command-pane-mixin that sets the theme background to a window header look.
                   The inspector editor uses a theme background.
2004-feb-01 Terje The single line minibuffer separates itself better from the editor text:
                   view-draw-contents on new-mini-buffer draws theme placard and calls next method (to write text) afterwards.
                   view-draw-contents on scrolling-fred-view no longer draws a frame around the view under osx.
2004-Jan-29 Terje Radio Buttons and Check Boxes are properly deactivated:
                   Added view-activate-event-handler and view-deactivate-event-handler for control-dialog-item.
2004-Jan-29 Terje Eliminates white ring around disabled editable-text-dialog-item:
                   Takes call to DrawThemeFocusRect out of with-fore-color body in view-draw-contents :after of basic-editable-text-dialog-item.
                   Separate out the appearance savvy code in view-draw-contents :after of basic-editable-text-dialog-item to make non-carbon disposable.
2004-Jan-29 Terje Makes Multi-pane-view and Tab-Bar-View of the Appearance Manager from MCLs Examples use Carbon if loaded. 
2004-Jan-28 Terje Simplified and improved drawing of scrolling-fred-view: 
                   Combined :around and :after methods of view-(de)activate-event-handler on scrolling-fred-view into main method.
                   Integrated :before method of view-deactivate-event-handler for scrolling-fred-view into main method to get better compatability with the color-coded.lisp contribution to MCL (suggested by Octav Popescu).
                   Converted :before method of view-activate-event-handler for scrolling-fred-view into a main method.
                   Set an inactive scrolling-fred-view's theme text color to kThemeTextColorDialogInactive (except for Fred Window).
                   view-deactivate-event-handler for scrolling-fred-view calls invalidate-view in OSX instead of invalidate-view-border.
                   view-corners for scrolling-fred-view insets three pixels in OSX to cover focus ring.
2004-Jan-22 Terje Underlined View deactivates properly
                    Removed osx-p test in view-(de)activate-event-handler unless MCL 5.
                    view-draw-contents on underlined-view sets theme text color.
2004-Jan-22 Terje Better Fred display when the window is inactive:
                    new-mini-buffer uses a theme brush for its delimiter line.
                    Added view-draw-contents for scrolling-fred-view with theme brush.
2004-Jan-21 Terje Eliminated appearance-fore-color function.
2004-Jan-21 Terje Use modern pop-up also forOS9:
                    Sets *use-pop-up-control* to true.
2004-Jan-20 terje Theme Cursors under Carbon
                    Redefinition of set-cursor function to process theme cursor keywords.
                    Bar dragger uses theme cursor for horizontal resize.
                    Theme cursor under Carbon for most controls and dialog-items.
2004-Jan-19 Terje Theme Fonts under Carbon
                    Redefinition of font-codes function to process theme font keywords.
                    The font-codes function allows later font spec items to override existing values.
                    %get-theme-font-values function to look up font values from a font ID.
                    *theme-font-alist* maps from theme font keywords to constants.
2004-Jan-19 Terje 3D Button has a theme appearance (benefitting the Inspector):
                    Added simplified text-position method for 3D button.
                    View-draw-text for 3D button both for carbon and before.
                    Added numerous methods to implement the use of theme brushes.
2004-Jan-19 Terje The Fred editor pane splitter has a new look under OSX:
                    Added view-draw-contents method for pane-splitter with theme savvy button background
                    and drag stripes instead of black rectangle.
2004-Jan-19 Terje Pop-up menu (de)activated in OSX:
                    New view-deactivate-event-handler :before method on pop-up-menu for carbon.
                    New view-activate-event-handler :before method on pop-up-menu for carbon.
2004-Jan-19 Terje Left-border-view erased on (de)activation in OSX.
2004-Jan-19 Terje Table Dialog Items more theme savvy:
                    Added new view-draw-contents on table-dialog-item for MCL 5. 
                    New %draw-table-cell-new grays out table text under OS9 for inactive windows.
                    Increased inset in view-corners on table-dialog-item under OSX.
                    invert-cell-selection invalidates selected area instead of hiliting it under Carbon.
                    Added view-click-event-handler for table-dialog-item.
                    New carbon initialize-instance :around for table-dialog-item provides better initarg defaults.
                    Added separator-size for table-dialog-item using size of ThemeBrushListViewSeparator as default.
                    Attempted to fix an OSX problem with the theme focus rect remaining when scrolling.
2004-Jan-19 Terje Make theme backgrounds work also under MacOS 9 by patching window-make-parts on window.
2004-Jan-16 Terje Underlined View has theme savvy text and proper appearance under Carbon/Aqua:
                    New Carbon version of view-draw-contents of underlined-view.
                    view-activate-event-handler :before of underlined-view simplified for MCL5.
                    view-deactivate-event-handler :before of underlined-view simplified for MCL5. 
2004-Jan-16 Terje Ellipsized Text Dialog Item has proper OSX appearance rather than partly platinum look:
                    Patched draw-theme-text-box so it doesn't fail when truncating the text.
                    Fixes flaw in MCL 5 where e.g. the text in the Search Files dialog displaying the file currently 
                    searched has a gray background (problem also applies to other uses of ellipsized-text-dialog-item).
2004-Jan-16 Terje Fixes the title-box-dialog-item to make it compliant with OSX/Aqua:
                    view-activate-event-handler :before of title-box-dialog-item simplified for MCL5.
                    view-deactivate-event-handler :before of title-box-dialog-item simplified for MCL5.
                    view-draw-contents uses DrawThemeTextBox instead of drawstring to get theme savvy anti-aliased text.
                    view-draw-contents eliminates the erase so that existing background remains.
                    view-draw-contents allows the title to be outside the box.
                    New label-offset for Carbon calls GetThemeTextDimensions.
                    label-offset moves title above the box for OSX as specified by Aqua (suggested by Octav Popescu).
                    New view-default-font for title-box-dialog-item.
                    update-title-box-width allows box wihtout title.
                    update-title-box-width calls GetThemeTextDimensions under Carbon. 
2004-Jan-16 Terje Changing the text of a static-text-dialog-item erases previous text as it should also in OSX:
                    set-dialog-item-text of static-text-dialog-item erases when invalidating view.
                    view-activate-event-handler :before of static-text-dialog-item simplified for MCL5.
                    view-deactivate-event-handler :before of static-text-dialog-item simplified for MCL5.
                    view-draw-contents :around of static-text-dialog-item only patched before MCL 5.                  
2003-Feb-03 Terje Fixed missing inside 'shadow' on basic-editable-text-dialog-item.
2003-Feb-03 Terje Version 1.3 released.
2003-jan-31 Terje Patched view-draw-contents on static-text-dialog-item is bypassed when OSX.
                  Modified view-draw-contents on scrolling-fred-view to loose the stripes in OSX.
                  Table text not grayed out in OSX.
                  Updated view-draw-contents on underlined-view with code from MCL kernel.
                  Updated view-draw-contents on title-box-dialog-item to not gray out on OSX.
                  New draw-horizontal-dragger function used for the bar dragger.
                  View-corners on table-dialog-item covers osx inset.
                  Scroll-bar-dialog-item displays deactivated scrollers under appearance.
2002-Oct-27 Terje View-draw-contents on static-text-dialog-item uses gray rather than pattern when disabled.
2002-Aug-07 Terje Added osx-p function definition.
2002-Aug-07 Terje Added test for installed-item-p in view-corners on view.
2002-jun-28 terje Eliminated use of obsolete #$kThemeStateDisabled.
2002-jun-11 Terje Version 1.2 released.
2002-mar-17 Terje Bar dragger and poof-button has placard appearance.
                  Eliminated focus rectangle when a scrolling view is part of fred editor.
2002-mar-16 Terje Table-dialog-item displays focus rectangle when key-handler.
                  Eliminated focus for arrow dialog items as it is inherited from table-dialog-item.
                  New appearance-theme-state function provides theme state code.
                  Corrects inactive drawing by using new appearance-theme-state function.
                  Scroll bar dialog item has better appearance when inactive.
2002-jan-04 Terje No longer defines appearance-manager-p etc for mcl 4.3.1 and beyond
2002-jan-04 Terje Adds tests for osx-p to avoid platinum appearance in osx.
2002-jan-04 Terje Carbon compatability copied from mcl source to view-draw-contents for table-dialog-item.
2001-may-26 Terje view-click-event-handler on fred-mixin tests for key-handler-p. 
1999-oct-21 Terje Version 1.1 released.
1999-oct-15 Terje Renamed from appearance-editable-text
1999-oct-15 Terje No longer requires appearance manager contribution.
1999-sep-19 Terje Different drawing depending on whether window is active.
1999-08-09 Terje  Eliminated unecessarry back colors when appearance (that caused white frame).
1999-08-09 Terje  view-draw-contents uses with-focused-dialog-item macro.
1999-08-09 Terje  view-draw-contents uses with-item-rect macro.
1999-07-10 Terje  No focus unless the dialog item is enabled. 
1999-07-09 Terje  Correct background color when no appearance.
1999-07-06 Terje  Version 1.0 released.

|#

(in-package :ccl)

(add-feature :alice) ; comment in/out depending on whether you want to adhere to Alice/Digitool's wishes for next gen MCL UI. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NEW COLOR:

(defparameter *lightest-gray-color* (+ (* 239 256 256) (* 239 256) 239))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance initialization borrowed from Eric Russel's Appearance module

#-ccl-4.3.1
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ccl::add-to-shared-library-search-path "AppearanceLib" t))

#-ccl-4.3.1
(unless (fboundp 'appearance-available-p)

 (defvar *appearance-available-p*)
 (defvar *appearance-compatibility-mode-p*)
 (defvar *DRAW-INACTIVE-DIALOG-ITEMS-AS-DISABLED* NIL)

 (defun appearance-available-p ()
   *appearance-available-p*)
)

#-ccl-4.3.1
(def-ccl-pointers appearance-available ()
  (let (#-carbon-compat (flags (gestalt #$gestaltAppearanceAttr)))
      (when #+carbon-compat T #-carbon-compat (and flags (logbitp #$gestaltAppearanceExists flags))
        (setq *appearance-available-p* t
              *appearance-compatibility-mode-p*
              (logbitp #$gestaltAppearanceCompatMode flags))
        #-carbon-compat
        (#_RegisterAppearanceClient)
        t)))

#+(and ccl-4.3.5 (not ccl-5.0))
(when (boundp '*appearance-compatibility-mode-p*)
  (setq *appearance-compatibility-mode-p* nil))

#-ccl-4.3.1
(defun osx-p () NIL)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Patch for MCL uses view-corners instead of view-size.
;; This allows certain view-draw functions to draw outside its regular area.

#| DISCUSSION:

Certain widgets in MacOS8/9 draws outside of their boundaries. This causes compatability problems with user interfaces designed for MacOS7, as unwanted clipping may occur. 

For example, many of the controls of MacOS8/9 has a focus ring drawn outside the bounding box. If an editable-text-dialog-item is placed in the top left corner of a view, parts of the focus ring will be clipped. As a consequence, composite dialog items such as the TYPEIN-MENU won't be properly drawn with Appearance.

The problem is also manifest without Appearance. The default-button-dialog-item already draws outside its bounding box. If a default-button-dialog-item is placed in the top left corner of a view, the ring around it will be clipped:

(make-instance 'window
  :view-subviews
    (list
      (make-instance 'view
        :view-position #@(30 30)
        :view-size #@(62 20)
        :view-subviews
         (list
           (make-instance 'default-button-dialog-item :view-position #@(0 0))))))

MCL allows a view to have a specialized view-corners method that adjust the view region to draw outside the boundaries of view-position and view-size. However, it doesn't adapt when a subview unexpectedly draws further outside its area. This may lead to a maintainance nightmare when dialog items are upgraded to draw further outside their boundaries than at the time of original implementation.

There are at least three ways to resolve the problem with MCL clipping MacOS8 controls:

1. REQUIRE THE DEVELOPERS TO UPDATE THE VIEW-CORNER METHOD OF ALL AFFECTED VIEWS. The affected views are any that uses dialog items that draw further outside their bounding box than in earlier versions of the MacOS, and that place these dialog items close to the sides of the view. The TYPEIN-MENU is one example of an affected view in MCL.

The disadvantages of this solution is that it will require a lot of modifications for various developers, and potentially cause third party modules to be outdated. It also won't solve the problem for the future, requiring similar modifications each time Apple changes the drawing of controls.

2. ADD A VIEW-CORNERS METHOD ON VIEW THAT TAKES INTO ACCOUNT EVENTUAL SUBVIEWS THAT HAS ITS VIEW CORNERS OUTSIDE THE BOX OF THE VIEW. That is, view corners call view corners on all subviews, and return corners that covers the view region of all subviews.

The benefit of this solution is that it resolves the problem without requiring further work by Digitool or third party developers. The solution will also cover future changes to the drawing of controls. On the other hand, large dialogs with many layers may have a noticable performance decrease in drawing and other activities that requires calculation of view corners.

3. PROVIDE A NEW CLASS FOR BUILDING VIEWS THAT ACT AS DIALOG ITEMS. This class could for example be called COMPOSITE-DIALOG-ITEM, and should have the same interface as a dialog items with the addition that it can have subviews. A composite dialog item should compute its view corners based on the view corners of its subviews, as described in solution 2. TYPEIN-MENU is an example of a composite dialog item that should be of this class.

An advantage of this approach is that it provides a long-term solution to the problem of dialog items drawing outside the boundaries of its container. It allows regular views to still be used for layout purposes, without the overhead of recomputing view corners to cover subviews. However, the work still has to be done to change affected views to be subviews of composite-dialog-item instead of view.

The code below implements solution [2] in the list above. I suggest that solution [3] is selected for the long term, incorporating the code from solution [2] in a composite-dialog-item used for all views that have subviews close to its sides.
|#

(in-package :ccl)

(let ((*warn-if-redefine* nil)
      (*warn-if-redefine-kernel* nil))

;; Fixes what appears to be a flaw in compute-view-region so that it takes into account the view-corners instead of using view-position and view-size:

(defmethod compute-view-region ((view view) rgn container)
  (when rgn
    (if container
     (multiple-value-bind (topleft bottomright)
      (view-corners view)
      (let* ((origin (view-origin-slot view))
             (container-origin (view-origin container))
             (tl (add-points topleft
                             (subtract-points origin container-origin)))
             (br (add-points tl (- bottomright topleft)))
             (offset (subtract-points container-origin origin))
             (offset-h (point-h offset))
             (offset-v (point-v offset))
             (container-region (view-clip-region container)))
        (#_SetRectRgn rgn (point-h tl) (point-v tl) (point-h br) (point-v br))
        (#_OffsetRgn rgn offset-h offset-v)
        (#_SectRgn rgn container-region rgn)
        (#_OffsetRgn rgn (- offset-h) (- offset-v))))
      (#_SetRectRgn rgn -32767 -32767 32767 32767)))
   rgn)

;; Adjusts if subviews goes outside the corners of the view:

(defmethod view-corners ((view view))
  (multiple-value-bind (topleft bottomright)
    (call-next-method)
    (let ((top-adjust 0)
          (left-adjust 0)
          (width (- (point-h bottomright) (point-h topleft)))
          (height (- (point-v bottomright) (point-v topleft)))) 
      (do-subviews (subview view)
       (when (installed-item-p subview)
        (multiple-value-bind (sub-topleft sub-bottomright)
          (view-corners subview)
          (when (< (point-h sub-topleft) left-adjust)
            (setf left-adjust (point-h sub-topleft)))
          (when (< (point-v sub-topleft) top-adjust)
            (setf top-adjust (point-v sub-topleft)))
          (when (> (point-h sub-bottomright) width)
            (setf width (point-h sub-bottomright)))
          (when (> (point-v sub-bottomright) height)
            (setf height (point-v sub-bottomright))))))
      (values (make-point (+ (point-h topleft) left-adjust)
                          (+ (point-v topleft) top-adjust))
              (make-point (+ width (point-h topleft))
                          (+ height (point-v topleft)))))))

) ;; redefine

#| This failed in MCL 4.4b4 unless view-corners on view uses installed-item-p to avoid adjusting for subview that isn't installed.

(defclass test-view (view)
())

(defmethod initialize-instance ((this test-view) &rest rest)
  (declare (ignore rest))
  (call-next-method)
  (add-subviews this
    (make-instance 'table-dialog-item)
    (make-instance 'button-dialog-item) 
    )
)

(make-instance 'window
  :view-subviews (list (make-instance 'test-view)))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; New: exit-key-handler notifies all its containers about the change, so that
;;; they can update accordingly. This makes sense when a view should display
;;; a focus rect but has one of its subviews as key handler (like scrolling-fred-view).

(let ((*warn-if-redefine* nil)
      (*warn-if-redefine-kernel* nil))

(defmethod enter-key-handler (view old-item)
  (let ((container (view-container view)))
    (when container
      (enter-key-handler container old-item))))

(defmethod exit-key-handler (view new-item)
  (let ((container (view-container view)))
    (if container
      (exit-key-handler container new-item)
      (progn
        ;; Set current key handler to NIL, so that :after methods can redraw without object 
        ;; still being the key handler. 
        ;; ## eliminates the MCL exit-key-handler for arrow-dialog-item that does the same!
        (setf (%get-current-key-handler (view-window view)) NIL)
        T))))

(defmethod enter-key-handler ((item key-handler-mixin) last-key-handler)
  (declare (ignore last-key-handler))
  (call-next-method))

(defmethod exit-key-handler ((item key-handler-mixin) next-key-handler)
  (declare (ignore next-key-handler))
  (call-next-method))

) ; end redefine

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-current-key-handler calls view-deactivate-event-handler and view-activate-event-handler.
;; These are documented to be called by the event system when the window containing
;; view is (de)activated and a different window is made active. They should thus not
;; be called when setting the key handler. 
;;
;; Calling these in set-current-key-handler results in that views with a different
;; appearance when deactivated will be redrawn multiple times when changing the key handler.

(let ((*warn-if-redefine* nil)
      (*warn-if-redefine-kernel* nil))

(defmethod set-current-key-handler ((dialog window) item &optional (select-all t)
                                      &aux old)
  (unless (or (null item)
              (and (memq item (%get-key-handler-list dialog))
                   (key-handler-p item)))
    (error "~s is either disabled or is not a key-handler item of ~s" item dialog))
  (if (and (neq item (setq old (%get-current-key-handler dialog)))
           (if old 
             (when (exit-key-handler old item)
               (multiple-value-bind (s e) (selection-range old)
                 (declare (ignore s))
                 ; do this first else display may be wrong.
                 (set-selection-range old e e))
               ; (view-deactivate-event-handler old)
               t)
             t))
    (without-interrupts
     (setf (%get-current-key-handler dialog) item)
     (when item
       (when select-all
         (set-selection-range item 0 most-positive-fixnum))
       ;(if (window-active-p dialog)
       ;  (view-activate-event-handler item))
       (enter-key-handler item old)))
    (when (and item (eq item old) select-all)
      (set-selection-range item 0 most-positive-fixnum)))
  item)

) ; end redefine

;; The FRED-MIXIN appears to be the only that is affected by the correction to set-current-key-handler. 
;; Its activate event handlers are responsible for updating the caret. This should rather be taken 
;; care of by specializations of exit- and enter-key-handler:

;; Same as view-deactivate-event-handler for fred-mixin, to eliminate caret
;; Perhaps view-activate-event-handler should be eliminated???

(defmethod exit-key-handler :after ((w fred-mixin) next)
  (declare (ignore next))
  (without-interrupts
   (let ((frec (frec w)))
     (with-focused-view w
       (with-text-colors w
         (frec-deactivate frec)
         (frec-update frec t))))))

;; Same as view-deactivate-event-handler for fred-mixin, to get caret
;; Perhaps view-deactivate-event-handler should be eliminated???

(defmethod enter-key-handler :after ((w fred-mixin) previous)
  (declare (ignore previous))
  (without-interrupts
   (let ((frec (frec w)))
     (with-focused-view w
       (with-text-colors w
         (frec-activate frec)
         ; draw the thing now before frec idle happens giving us half caret
         (frec-update frec))))))
         ;(frec-update frec t))))))         ; redraw selection box

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ACTIVATE AND DEACTIVATE CONTROLS
;;
;; Affects the radio-button-dialog-item and the check-box-dialog-item.

(unless (module-loaded-p :appearance-activity-mixin) ; appearance-activity-mixin.lisp in the MCL Examples folder

(defmethod view-activate-event-handler ((view control-dialog-item))
  (call-next-method)
  (when (and
         #-carbon-compat (appearance-available-p)
         (dialog-item-enabled-p view))
    (#_ActivateControl (dialog-item-handle view))))

(defmethod view-deactivate-event-handler ((view control-dialog-item))
  (when (and
         #-carbon-compat (appearance-available-p)
         (dialog-item-enabled-p view))
    (#_DeactivateControl (dialog-item-handle view)))
  (call-next-method))

) ; end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DRAW-ACTIVE-P
;; (Idea from Appearance-Activity-Mixin by Eric Russell)

(unless (fboundp 'draw-active-p) ; in case appearance-activity-mixin is loaded or these are defined elsewhere...
 
(defmethod draw-active-p ((view simple-view) &aux (window (view-window view)))
  (and window (window-active-p window)))

(defmethod draw-active-p ((view dialog-item) &aux (window (view-window view)))
  (and window (window-active-p window) (dialog-item-enabled-p view)))

) ; end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; APPEARANCE THEME STATE

(defun appearance-theme-state (view)
  (cond
   ;((and (typep view 'dialog-item)
   ;      (not (dialog-item-enabled-p view)))
   ; #$kThemeStateInactive)
   ((draw-active-p view)
    #$kThemeStateActive)
   (T
    #$kThemeStateInactive)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make theme backgrounds work also under MacOS 9.
;; Digitool covers this in the theme-patch.lisp for MCL 5, and will likely include it in later versions of MCL.

(let ((*warn-if-redefine* nil)
      (*warn-if-redefine-kernel* nil))
#+(and ccl-4.3.5 (not ccl-5.1));; need to verify that it hasn't changed for versions after mcl5 (if it even is needed)!
(unless (module-loaded-p :theme-patch)
(defmethod window-make-parts ((window window)
                              &key (view-position (view-default-position window) pos-p)
                              (view-size (view-default-size window) size-p)
                              (window-type :document-with-zoom wtype-p)
                              back-color
                              content-color
                              theme-background
                              procid
                              (window-title  "Untitled")
                              (close-box-p t)
                              (color-p t)
                              (grow-icon-p nil gip?))
  (unless (wptr window)
    (if procid (setq gip? nil grow-icon-p nil))
    (when gip?
      (if grow-icon-p
        (cond ((eq window-type :document) (setq window-type :document-with-grow))
              ((eq window-type :windoid) (setq window-type :windoid-with-grow))
              ((eq window-type :windoid-side)(setq window-type :windoid-side-with-grow))
              ((eq window-type :windoid-with-zoom)(setq window-type :windoid-with-zoom-grow))
              ((eq window-type :windoid-side-with-zoom)(setq window-type :windoid-side-with-zoom-grow))
              ((not (memq  window-type '(:document-with-grow :document-with-zoom
                                         :windoid-with-grow :windoid-with-zoom-grow :windoid-side-with-grow
                                         :windoid-side-with-zoom-grow)))
               (setq gip? nil grow-icon-p nil)))
        (cond ((eq window-type :document-with-grow) (setq window-type :document))
              ((eq window-type :document-with-zoom) (setq window-type :document-with-zoom-no-grow))
              ((eq window-type :windoid-with-grow) (setq window-type :windoid))
              ((eq window-type :windoid-side-with-grow)(setq window-type :windoid-side))
              ((eq window-type :windoid-side-with-zoom-grow)(setq window-type :windoid-side-with-zoom))
              ;; this is wrong?
              ((not (memq window-type '(:document :document-with-zoom)))
               (setq gip? nil grow-icon-p nil)))))
    (when wtype-p
      (when (and (not (typep window 'windoid)) (memq window-type *windoid-types*))
        ;(error "Need to make a windoid for window-type ~s." window-type)
        (change-class window 'windoid)        
        (when (not pos-p)(setq view-position (view-default-position window)))
        (when (not size-p)(setq view-size (view-default-size window)))
        )
      (when nil ;(and (typep window 'windoid)(not (memq window-type *windoid-types*)))
        (report-bad-arg window-type (cons 'member *windoid-types*))))
    (let* ((wptr (%new-window (or procid window-type)
                              view-position
                              view-size
                              close-box-p
                              nil
                              color-p))
           (procid #-carbon-compat (rref wptr windowrecord.refCon) #+carbon-compat (#_getwrefcon wptr)))   ; %new-window leaves it there
      (setf (wptr window) wptr)      
      #+ignore ;; - too slow
      (when (and nil (not pos-p) (eql view-position *window-default-position*))
        (set-view-position window #@(-3000 -3000))
        (window-show window)
        (let ((left-border (window-border-width window))
              (title-height (window-title-height window)))
          (window-hide window)
          (set-view-position window (make-point (max (1+ left-border)(point-h view-position))
                                                (max (+ title-height 2 (menubar-height)) (point-v view-position))))))
      (set-window-title window window-title)
      (setf (slot-value window 'grow-icon-p)
            (if gip? grow-icon-p (memq procid  *grow-procids*)))
      (when content-color  ;; is this used for anything?
        (set-part-color window :content content-color)
        ;(set-part-color window :title-bar *white-color*) doesnt help
        )
      (when back-color
        (setf (slot-value window 'back-color) back-color)  ; <<
        (set-back-color window back-color))
      (when (and theme-background #+ignore(osx-p))
        (view-put window 'theme-background 
                  (if (eq theme-background t)
                    (setq theme-background #$kThemeBrushModelessDialogBackgroundActive)
                    theme-background))
        (#_SetThemeWindowBackground wptr theme-background t))
      #-CCL-4.3.5 ;;; ??
      (when (and (osx-p) ;; ??
                 close-box-p
                 (not (typep window 'windoid))
                 (not (slot-value window 'grow-icon-p))
                 (not (wptr-dialog-p wptr)))
        (Set-bubble-attributes window #$kWindowCollapseBoxAttribute))
        
      #+carbon-compat
      (if (typep window 'windoid)
        (#_setwindowclass wptr #$kFloatingWindowClass )
        (when (and (wptr-dialog-p wptr)) ;(find-class 'drag-receiver-dialog nil)(typep window 'drag-receiver-dialog))
          ; make it non-modal till actually used modally - for IFT or for everybody
          (#_setwindowclass wptr #$kDocumentWindowClass )  ;; do we really need both of these?
          (setwindowmodality wptr #$kWindowModalityNone)
          )))))

)) ; end redefine

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; THEME FONTS
;; Supports theme font keywords as part of MCL font descriptions.

;; ## I'd also like to see support for :larger and :smaller keywords in the spec, which respectively increase/decrease fontsize.

#+carbon-compat
(defparameter *theme-font-alist* ; 
  `(; -- Appearance 1.1 and later:
    (:system-font . #.#$kThemeSystemFont)
    (:small-system-font . #.#$kThemeSmallSystemFont)
   (:small-emphasized-system-font . #.#$kThemeSmallEmphasizedSystemFont)
   (:views-font . #.#$kThemeViewsFont)
   (:emphasized-system-font . #.#$kThemeEmphasizedSystemFont)
   ; -- OSX and CarbonLib 1.3:
   #+carbon-compat
   (:application-font . #.#$kThemeApplicationFont)
   #+carbon-compat
   (:label-font . #.#$kThemeLabelFont)
   #+carbon-compat
   (:menu-title-font . #.#$kThemeMenuTitleFont)
   #+carbon-compat
   (:menu-item-font . #.#$kThemeMenuItemFont)
   #+carbon-compat
   (:menu-item-mark-font . #.#$kThemeMenuItemMarkFont)
   #+carbon-compat
   (:menu-item-cmd-key-font . #.#$kThemeMenuItemCmdKeyFont)
   #+carbon-compat
   (:window-title-font . #.#$kThemeWindowTitleFont)
   #+carbon-compat
   (:push-button-font . #.#$kThemePushButtonFont)
   #+carbon-compat
   (:utility-window-title-font . #.#$kThemeUtilityWindowTitleFont)
   #+carbon-compat
   (:alert-header-font . #.#$kThemeAlertHeaderFont)
 ;  (:system-font-detail . #.#$kThemeSystemFontDetail)
 ;  (:system-font-detail-emphasized . #.#$kThemeSystemFontDetailEmphasized)
   (:current-port-font . #.#$kThemeCurrentPortFont)
   ; -- OSX 1.2 and later
 ;  (:toolbar-font . #.#$kThemeToolbarFont)
))

#+carbon-compat
(defun %get-theme-font-values (font-id &optional script)
  "Looks up the font from the id and returns its code, size and style as values"
  (rlet ((name (:string 255))
         (size :word)
         (style :style))
    (errchk (#_getThemeFont 
             (or font-id #$kThemeSystemFont) 
             (or script #$smSystemScript)
             name size style))
    (values
     (#_FMGetFontFamilyFromName name) ; (font-number-from-name (%get-string name))
     (%get-word size)
     (#.(mactype-get-function (find-mactype :style)) style))))

#+carbon-compat
(let ((*warn-if-redefine* nil)
      (*warn-if-redefine-kernel* nil))

(defun font-codes (font-spec &optional old-ff old-ms
                             &aux 
                             (items font-spec) temp item font face mode size color
                             reset-style-p 
                             (font-mask 0) (face-mask 0) (color-mask 0)
                             (mode-mask 0) (size-mask 0))
  (if (null old-ff) (setq old-ff 0))
  (if (null old-ms) (setq old-ms (make-point 0 (xfer-mode-arg :srcor)))) ;;maybe should be #$transparent?? prob not
  (if (null font-spec)
    (return-from font-codes (values old-ff old-ms 0 0)))
  (setq item (if (consp items) (pop items) items))
  (tagbody
    LOOP
    (cond
     ((fixnump item)
      ;(if size 
      ;  (error "Font Spec: ~s contains two sizes" font-spec)
        (setq size item
              size-mask -1));)
     ((stringp item)
      ;(if font (error "Font Spec: ~s contains two strings" font-spec))
      (setq font-mask -1)
      (if (equalp item (car (sys-font-spec)))
        (setq font (ash (car *sys-font-codes*) -16))  ; in OS 8 its the real font-num - earlier it's 0 
        (let ((num (font-number-from-name item)))
          ;; so what do you do if it doesnt exist?
          (setq font
                (or num
                    (ash (car *sys-font-codes*) -16))))))
     ((consp item)
      (ecase (car item)
        (:color-index
         ;(when color
         ;  (error "Font Spec: ~s contains two color specs" font-spec))
         (setq color (second item))
         (unless (and (fixnump color)
                      (<= 0 color 255))
           (error "~s is not a valid font color" color))
         (setq color-mask 255))
        (:color
         ;(when color
         ;  (error "Font Spec: ~s contains two color specs" font-spec))
         (setq color (fred-palette-closest-entry (second item))
               color-mask 255))))
     ((setq temp (xfer-mode-arg item))
      ;(if mode 
      ;  (error "Font Spec: ~s contains two text-modes" font-spec)
        (setq mode temp
              mode-mask -1));)
     ((setq temp (assq item *style-alist*))
      (when (eq (%car temp) :plain)
        (setq reset-style-p t
              face-mask -1))
      (setq temp (%cdr temp))
      (setq face (if face (%ilogior2 face temp) temp)
            face-mask (%ilogior2 face-mask temp)))
     ((setq temp (assq item *theme-font-alist*))
      (multiple-value-setq (font size face) (%get-theme-font-values (%cdr temp))))
     (t (error "Unrecognized option ~a in font-spec: ~a" item font-spec)))
    (if (consp items) (progn (setq item (pop items)) (go LOOP))))
  (unless font (setq font (point-v old-ff)))
  (unless face (setq face (%ilsr 8 (point-h old-ff))))
  (unless color (setq color (%ilogand 255 (point-h old-ff))))
  (unless reset-style-p
    (setq face (%ilogior2 face (%ilsr 8 (point-h old-ff)))))
  (unless mode (setq mode (point-v old-ms)))
  (unless size (setq size (point-h old-ms)))
  (values (make-point (+ color (%ilsl 8 face)) font)
          (make-point size mode)
          (make-point (logior color-mask (%ilsl 8 face-mask)) font-mask)
          (make-point size-mask mode-mask)))

) ; end redefine

#|
(make-instance 'window
  :theme-background T
  :view-subviews 
  (list 
   (make-dialog-item 'static-text-dialog-item #@(10 10) nil "Theme Font" nil 
                     :view-font '(:system-font :bold))
   (make-dialog-item 'static-text-dialog-item #@(10 30) nil "Theme Font" nil 
                     :view-font '(:application-font 18))
   (make-dialog-item 'static-text-dialog-item #@(10 60) nil "Theme Font" nil 
                     :view-font `(:views-font))
   (make-dialog-item 'static-text-dialog-item #@(10 90) nil "Theme Font" nil 
                     :view-font :small-emphasized-system-font)))

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; THEME BRUSHES

(defparameter *theme-brushes-alist* ; just a start...
  `((:focus-highlight-brush . ,#$kThemeBrushFocusHighlight)
    (:button-frame-active-brush . ,#$kThemeBrushButtonFrameActive)
    (:button-frame-inactive-brush . ,#$kThemeBrushButtonFrameInactive)
    (:button-face-active-brush . ,#$kThemeBrushButtonFaceActive)
    (:button-face-inactive-brush . ,#$kThemeBrushButtonFaceInactive)
    (:button-face-pressed-brush . ,#$kThemeBrushButtonFacePressed)
    (:button-active-dark-shadow-brush . ,#$kThemeBrushButtonActiveDarkShadow)
    (:button-active-dark-highlight-brush . ,#$kThemeBrushButtonActiveDarkHighlight)
    (:button-active-light-shadow-brush . ,#$kThemeBrushButtonActiveLightShadow)
    (:button-active-light-highlight-brush . ,#$kThemeBrushButtonActiveLightHighlight)
    (:button-inactive-dark-shadow-brush . ,#$kThemeBrushButtonInactiveDarkShadow)
    (:button-inactive-dark-highlight-brush . ,#$kThemeBrushButtonInactiveDarkHighlight)
    (:button-inactive-light-shadow-brush . ,#$kThemeBrushButtonInactiveLightShadow)
    (:button-inactive-light-highlight-brush . ,#$kThemeBrushButtonInactiveLightHighlight)
    (:button-pressed-dark-shadow-brush . ,#$kThemeBrushButtonPressedDarkShadow)
    (:button-pressed-dark-highlight-brush . ,#$kThemeBrushButtonPressedDarkHighlight)
    (:button-pressed-light-shadow-brush . ,#$kThemeBrushButtonPressedLightShadow)
    (:button-pressed-light-highlight-brush . ,#$kThemeBrushButtonPressedLightHighlight)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; THEME CURSORS

#+carbon-compat
(defparameter *theme-cursor-alist*
  `((:arrow-cursor . ,#$kThemeArrowCursor)
    (:contextual-menu-arrow-cursor . ,#$kThemeContextualMenuArrowCursor)
    (:alias-arrow-cursor . ,#$kThemeAliasArrowCursor)
    (:copy-arrow-cursor . ,#$kThemeCopyArrowCursor)
    (:I-beam-cursor . ,#$kThemeIBeamCursor)
    (:cross-cursor . ,#$kThemeCrossCursor)
    (:plus-cursor . ,#$kThemePlusCursor) ; discouraged for OSX
    (:watch-cursor . ,#$kThemeWatchCursor)
    (:closed-hand-cursor . ,#$kThemeClosedHandCursor)
    (:open-hand-cursor . ,#$kThemeOpenHandCursor)
    (:pointing-hand-cursor . ,#$kThemePointingHandCursor)
    (:counting-up-hand-cursor . ,#$kThemeCountingUpHandCursor) ; discouraged for OSX
    (:counting-down-hand-cursor . ,#$kThemeCountingDownHandCursor) ; discouraged for OSX
    (:counting-up-and-down-hand-cursor . ,#$kThemeCountingUpAndDownHandCursor) ; discouraged for OSX
    (:spinning-cursor . ,#$kThemeSpinningCursor) ; discouraged for OSX
    (:resize-left-cursor . ,#$kThemeResizeLeftCursor)
    (:resize-right-cursor . ,#$kThemeResizeRightCursor)
    (:resize-left-right-cursor . ,#$kThemeResizeLeftRightCursor)
    (:not-allowed-cursor . #+ccl-5.2 ,#$kThemeNotAllowedCursor #-ccl-5.2 18)
    #+ccl-5.2 
    (:resize-up-cursor . ,#$kThemeResizeUpCursor)
    #+ccl-5.2
    (:resize-down-cursor . ,#$kThemeResizeDownCursor)
    #+ccl-5.2
    (:resize-up-down-cursor . ,#$kThemeResizeUpDownCursor)
    #+ccl-5.2
    (:poof-cursor . ,#$kThemePoofCursor)
    ))

(let ((*warn-if-redefine* nil)
      (*warn-if-redefine-kernel* nil))
#+carbon-compat
(defun set-cursor (cursor)
  "If the argument is the wrong type this does a no-op"
  "if macptr but not handle may crap"
  (let ((temp *current-cursor*))
    (without-interrupts
     (typecase cursor
       (fixnum
        (with-macptrs ((temp2 (#_GetCursor cursor)))
          (unless (%null-ptr-p temp2)
            (#_SetCursor (%setf-macptr temp (%get-ptr temp2))))))
       (keyword
        (let ((value (cdr (assq cursor *theme-cursor-alist*))))
          (when value ; consider to err if no value!
            (#_SetThemeCursor value))))
       (otherwise
        (when (and cursor (if (osx-p) (macptrp cursor)(pointerp cursor)) (not (%null-ptr-p cursor)))
          (#_SetCursor (if (not (eql cursor *arrow-cursor*))      ; special case - today cursors are handles or fixnums
                         (progn (#_LoadResource cursor)
                                (%setf-macptr temp (%get-ptr cursor)))
                         (%setf-macptr temp cursor)))))))))

; cannot just modify constants like *arrow-cursor* as they might be used directly with #_SetCursor!
; Digitool may consider to substitute calls to (#_SetCursor *arrow-cursor*) with a call to set-cursor.

#+carbon-compat
(defmethod view-cursor ((item arrow-dialog-item) where)
  (declare (ignore where))
  :arrow-cursor)

#+carbon-compat
(defmethod view-cursor ((item control-dialog-item) where)
  (declare (ignore where))
  :arrow-cursor)

#+carbon-compat
(defmethod view-cursor ((w fred-mixin) point)
  (let* ((c (call-next-method))
         (frec (frec w)))
    (if (and (eq c :i-beam-cursor)(frec-up-to-date-p frec))
      (with-font-codes nil nil
        (frec-cursor (frec w) point))
      c)))

#+carbon-compat
(defmethod view-cursor ((item key-handler-mixin) point)
  (declare (ignore point))
  (let ((w (view-window item)))
    (if (and w (eq item (current-key-handler w))) 
      :i-beam-cursor
      :arrow-cursor)))

#+carbon-compat
(defmethod view-cursor ((v simple-view) point)
  (let ((container (view-container v)))
    (if container
      (view-cursor container (convert-coordinates point v container))
      :arrow-cursor)))

) ; end redefine

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Support deactivation of windows with themes.
;; Not used for now as they aren't yet supported by MacOSX anyway.

#|

(defmethod set-window-theme-background ((window window) new)
  "Update the theme for the current window" 
  (when (eq new t)
    (setq new (window-default-theme window)))
  (let ((old (view-get window 'theme-background)))
    (unless (eql new old)
      (let ((wptr (wptr window)))      
        (view-put window 'theme-background new)  
        (errchk (#_SetThemeWindowBackground wptr new nil)))
      new)))

(defmethod window-default-theme ((w window))
  #$kThemeBrushModelessDialogBackgroundActive)

(defparameter *theme-brush-window-background-map*
  `((,#$kThemeBrushDialogBackgroundActive .
     ,#$kThemeBrushDialogBackgroundInactive)
    (,#$kThemeBrushAlertBackgroundActive . 
     ,#$kThemeBrushAlertBackgroundInactive)
    (,#$kThemeBrushModelessDialogBackgroundActive .
     ,#$kThemeBrushModelessDialogBackgroundInactive)
    (,#$kThemeBrushUtilityWindowBackgroundActive .
     ,#$kThemeBrushUtilityWindowBackgroundInactive)))

;; add to existing methods on window:

(defmethod view-activate-event-handler :around ((window window))
  (let* ((old (view-get window 'theme-background))
         (new (car (rassoc old *theme-brush-window-background-map*))))
    (when (and new (not (eql old new)))
      (set-window-theme-background window new)))
  (call-next-method))

(defmethod view-deactivate-event-handler :around ((window window))
  (let* ((old (view-get window 'theme-background))
         (new (cdr (assoc old *theme-brush-window-background-map*))))
    (when (and new (not (eql old new)))
      (set-window-theme-background window new)))
  (call-next-method))

; (make-instance 'window :theme-background T)
; (make-instance 'window)


|# 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Support for custom background and text color for controls.

#+carbon-compat
(defvar handle->dialog-item (make-hash-table :weak :value))

#+carbon-compat
(defmethod (setf dialog-item-handle) :after (handle (item control-dialog-item))
  (setf (gethash handle handle->dialog-item) item))

#+carbon-compat
(defmethod view-setup-background (view depth color-p)
  (when view
    (view-setup-background (view-container view) depth color-p)))

#+carbon-compat
(defmethod view-setup-background ((item dialog-item) depth color-p)
  (let ((background-color (part-color item :back)))
    (cond
     (background-color
      (#_SetThemeBackground #$kThemeBrushWhite Depth color-p) ; hack to allow overriding the theme brush with custom color
      (with-rgb (rec background-color)
        (require-trap #_rgbBackColor rec))
      T)
     (T
      (call-next-method)))))

#+carbon-compat ; should text also inherit color from container?
(defmethod view-apply-text-color (view depth color-p)
  (declare (ignore view depth color-p))
  NIL)

#+carbon-compat
(defmethod view-apply-text-color ((item dialog-item) depth color-p)
  (declare (ignore depth))
  (when color-p
    (let ((color (part-color item :text)))
      (when color
        (with-rgb (rec color)
          (#_rgbForeColor rec))
        T))))

#+carbon-compat
(add-pascal-upp-alist 'control-color-proc
                      #'(lambda (procptr)(#_NewControlColorUPP procptr)))

#+carbon-compat
(defpascal control-color-proc (:pointer ControlHandle
                               :word Message ; sInt16
                               :word DrawDepth
                               :Boolean isColorDev
                               :word)
    (let ((view (gethash Controlhandle handle->dialog-item)))
      (if
        (case message
          (#.#$kControlMsgSetupBackground
           (view-setup-background view drawdepth iscolordev))
          (#.#$kControlMsgApplyTextColor ;; appearance 1.1
           (view-apply-text-color view drawdepth iscolordev)))
        #$noErr
        #$paramErr)))
  
#+carbon-compat
(defmethod install-view-in-window :after ((item control-dialog-item) window &aux (handle (dialog-item-handle item)))
  (declare (ignore window))
  (when handle
    (#_SetControlColorProc handle control-color-proc)))

#|

(defmethod redraw-color-dialog-item ((item check-box-dialog-item))
  nil)

(defmethod view-draw-contents ((item check-box-dialog-item))
  ;(when (installed-item-p item)
   ; (without-the-text-if-osx item 
  (call-next-method))  ;))

(make-instance 'window
  :theme-background T
  :view-subviews
  (list
   (make-instance 'check-box-dialog-item 
        :view-position #@(10 10)
        :dialog-item-text "Test" 
        :part-color-list `(:text ,*yellow-color* :back ,*blue-color*))))

|#

#| The code above and a view-setup-background for multi-pane-view fixes the following problem...
   This demo should result in a pane where the check box dialog item have a different background than the pane.

(progn
(require :appearance-manager "ccl:examples;appearance-manager-folder;appearance-manager.lisp")
(require :multi-pane-view "ccl:examples;appearance-manager-folder;multi-pane-view.lisp")
)

(let ((w (make-instance 'color-dialog
           :theme-background T))
      (pane-1 (make-instance 'view
                :view-subviews (list 
                                (make-instance 'check-box-dialog-item
                                  ; :part-color-list `(:text ,*blue-color*)
                                  ; :view-font :small-emphasized-system-font
                                  :view-position #@(20 20)
                                  :dialog-item-text "Check"))))
      (pane-2 (make-instance 'view
                :view-subviews (list
                                (make-instance 'button-dialog-item
                                  :view-position #@(32 20)
                                  :dialog-item-text "Button"))))
      (pane-3 (make-instance 'view
                :view-subviews (list
                                (make-instance 'radio-button-dialog-item
                                  :view-position #@(32 10)
                                  :dialog-item-text "Radio"))))
      (view   (make-instance 'multi-pane-view
                :view-position #@(20 30)
                :view-size     #@(300 120)
                ;:tab-font      '("Chicago" 12)
                ; :tab-bar-height 22
                )))
    (add-pane view pane-1 "One")
    (add-pane view pane-2 "Two")
    (add-pane view pane-3 "Three")
    (set-view-container view w))

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTIONS TO SET THEME BRUSHES

#+carbon-compat
(defun view-pixel-depth (view) ; compare to the screen-bits function
  (let ((portpixmap (#_getportpixmap (#_getwindowport (wptr view)))))
    (href portpixmap :pixmap.pixelsize)))

#+carbon-compat
(defun view-color-p (view)
  (wptr-color-p (wptr view)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MAKE STATIC TEXT DIALOG APPEARANCE SAVVY

(let ((*warn-if-redefine* nil)
      (*warn-if-redefine-kernel* nil))

#-ccl-5.0
(defmethod view-draw-contents :around ((item static-text-dialog-item))
  (if (#+carbon-compat osx-p)
    (call-next-method)
    (when (installed-item-p item)
      (with-focused-dialog-item (item)
        (let ((position (view-position item))
              (size (view-size item))
              (handle (dialog-item-handle item)))
          (with-slot-values (color-list text-justification #+ignore (enabled-p dialog-item-enabled-p))
                            item
            (rlet ((rect :rect)
                   ; (ps :penstate)
                   )
              (rset rect rect.topleft position)
              (rset rect rect.bottomright (add-points position size))
              (setq text-justification
                    (or (cdr (assq text-justification
                                   '((:left . #.#$tejustleft)
                                     (:center . #.#$tejustcenter)
                                     (:right . #.#$tejustright))))
                        (require-type text-justification 'fixnum)))
              (with-pointers ((tp handle))
                (with-fore-color (if (and 
                                      (ignore-errors (appearance-available-p))
                                      (not (draw-active-p item)))
                                   *gray-color*
                                   (getf color-list :text nil))
                  (with-back-color (getf color-list :body nil)
                    (#_TETextBox tp (#_GetHandleSize handle) rect text-justification))))
              #+ignore
              (unless enabled-p
                (#_GetPenState ps)
                (#_PenPat *gray-pattern*)
                (#_PenMode 11)
                (#_PaintRect rect)
                (#_SetPenState ps)))))))))

#+ccl-5.0 ; Based on version in Digitool's theme-patch.lisp for MCL 5 (likely same as in MCL 5.1)
(defmethod view-draw-contents ((item static-text-dialog-item))
  (when (installed-item-p item)
    (with-focused-dialog-item (item)
      (let ((position (view-position item))
            (size (view-size item))
            #|(handle (dialog-item-handle item))|#)
        (with-slot-values (color-list text-justification #|(enabled-p dialog-item-enabled-p)|#)
          item
          (rlet ((rect :rect) ;; # can use with-item-rect instead!!
                 #|(ps :penstate)|#)
            (rset rect rect.topleft position)
            (rset rect rect.bottomright (add-points position size))
            (setq text-justification
                  (case text-justification
                 #| (or (cdr (assq text-justification 
                                 '(|#
                    (:left #$tejustleft)
                    (:center #$tejustcenter)
                    (:right #$tejustright)
                    (otherwise (require-type text-justification 'fixnum))))
            (progn ;with-pointers ((tp handle)) ;; blech - the text is in the handle too
              (let ((back (getf color-list :body nil)))                
                (with-fore-color (or (getf color-list :text nil) *black-color*)
                  (with-back-color back
                    ;(if (and #|(osx-p)|# (null back)(theme-background-p (view-window item)))
                      (with-cfstrs ((cftext (dialog-item-text item)))  ;; n.b. font is semi ignored no longer
                        ;(multiple-value-bind (ff ms)(view-font-codes item)
                        ;  (let ((font-foo (if (> (logand ms #xffff) 10) #$kthemesystemfont 
                        ;                     (if (eq (ash (logand ff #xffff) -8) (cdr (assoc :bold *style-alist*))) ;; aka bold
                        ;                       #$kThemeSmallemphasizedSystemFont  ;; boy is that ugly
                        ;                       #$kThemeSmallSystemFont))))
                        (unless (getf color-list :text nil)
                                                    (#_SetThemeTextColor 
                           (if (draw-active-p item) #$kThemeTextColorDialogActive #$kThemeTextColorDialogInactive)
                           (view-pixel-depth item)
                           (view-color-p item)))
                        (#_Drawthemetextbox cftext #$kThemeCurrentPortFont (appearance-theme-state item) t rect text-justification *null-ptr*))
                      ;(with-dereferenced-handle (tp handle)
                      ;  (#_TEtextbox tp (#_GetHandleSize handle) rect text-justification)))
                  ))))
            #+ignore
            (unless enabled-p
              (#_GetPenState ps)
              (#_PenPat *gray-pattern*)
              (#_PenMode 11)
              (#_PaintRect rect)
              (#_SetPenState ps))))))))

;; perhaps invalidate-view on simple-view should be changed like this to have a true default value for erase-p?
;; Calls to invalidate-view can be changed accordingly!

#+ccl-5.0
(defmethod invalidate-view ((view static-text-dialog-item) &optional (erase-p t)) ;; erasing is required also in OS9!
  (call-next-method view erase-p))

(defmethod view-activate-event-handler :before ((item static-text-dialog-item))
  #-ccl-5.0
  (when (and (appearance-available-p)
             (not (osx-p)))
    (invalidate-view item))
  #+ccl-5.0
  (invalidate-view item)) ;; erasing is required also in OS9!

(defmethod view-deactivate-event-handler :before ((item static-text-dialog-item))
  #-ccl-5.0
  (when (and (appearance-available-p)
             (not (osx-p)))
    (invalidate-view item))
  #+ccl-5.0
  (invalidate-view item)) ;; erasing is required also in OS9!

#+(and ccl-5.0 (not ccl-5.1))
(defmethod set-dialog-item-text ((item static-text-dialog-item) text)
  (setq text (ensure-simple-string text))
  (let ((handle (dialog-item-handle item)))
    (when handle
      (%str-to-handle text handle)
      (invalidate-view item T #|(osx-p)|#)) ;; erasing is required also in OS9!
    (setf (slot-value item 'dialog-item-text) text))
  text)

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MAKE EDITABLE TEXT APPEARANCE SAVVY

(let ((*warn-if-redefine* nil)
      (*warn-if-redefine-kernel* nil))

 (defmethod view-draw-contents :after ((item basic-editable-text-dialog-item))
  (let (; (item-position (view-position item))
        ; (item-size (view-size item))
        #-carbon-compat
        (colorp (color-or-gray-p item)))
   (with-slot-values (dialog-item-enabled-p draw-outline) item
    (with-focused-dialog-item (item)
     (with-item-rect (rect item)
       (if #+carbon-compat T #-carbon-compat (appearance-available-p)
         (when draw-outline
           (let ((focus? (and dialog-item-enabled-p
                              (draw-active-p item)
                              (eq (window-key-handler (view-window item)) 
                                  item))))
             (#_insetRect rect -1 -1) ;; adjust if necessarry
             (unless focus? 
               (#_DrawThemeFocusRect rect NIL))
             ;; # Ignores :frame part color under Carbon, but maybe it should be supported?
             (#_DrawThemeEditTextFrame rect (appearance-theme-state item))
             (when focus? 
               (#_DrawThemeFocusRect rect T))))
         #-carbon-compat
         (rlet ((ps :penstate))
           (#_GetPenState ps)
           (unwind-protect
             (progn
               (when draw-outline
                 (let ((rgn1 *temp-rgn*)
                       (rgn2 *temp-rgn-2*)
                       (inset (if (fixnump draw-outline) draw-outline -3)))
                   (#_RectRgn rgn1 rect)
                   (#_insetRect rect inset inset) ; I like -2 -2 better
                   (#_RectRgn rgn2 rect)
                   (#_DiffRgn rgn2 rgn1 rgn1)
                   (#_PenNormal)
                   (unless (or dialog-item-enabled-p colorp)
                     (#_PenPat *gray-pattern*))                    
                   (with-fore-color (if (and colorp (not dialog-item-enabled-p)) 
                                      *gray-color* 
                                      (part-color item :frame))
                     (with-back-color (part-color item :body)
                       (#_EraseRgn rgn1)                   
                       (#_FrameRect rect))))
                 (#_insetRect rect 1 1)))
             (unless (or colorp dialog-item-enabled-p)
               (#_PenPat *gray-pattern*)
               (#_PenMode 11)
               (#_PaintRect rect)))
           (#_SetPenState ps))))))
))

) ; end redefine

(defmethod view-corners :around ((item basic-editable-text-dialog-item))
  (if (and (slot-value item 'draw-outline) (appearance-available-p))
    (multiple-value-call #'inset-corners #@(-3 -3) (call-next-method))
    (call-next-method)))

(defmethod exit-key-handler :after ((item basic-editable-text-dialog-item) new-item)
  (declare (ignore new-item))
  (when (and (slot-value item 'draw-outline) (appearance-available-p))
    (invalidate-view-border item)
    (view-focus-and-draw-contents item)))

(defmethod enter-key-handler :after ((item basic-editable-text-dialog-item) old-item)
  (declare (ignore old-item))
  (when (and (slot-value item 'draw-outline) (appearance-available-p))
    ; (invalidate-view-border item)
    (view-focus-and-draw-contents item)))

(defmethod view-activate-event-handler :before ((item basic-editable-text-dialog-item))
  (when (appearance-available-p)
    (invalidate-view item (osx-p))))

(defmethod view-deactivate-event-handler :after ((item basic-editable-text-dialog-item))
  (when (appearance-available-p)
    (invalidate-view item (osx-p))))

#-ccl-5.1 ; covered by view-draw-contents on fred-dialog-item
(defmethod view-draw-contents :around ((item basic-editable-text-dialog-item))
  (if (and (not (osx-p))
           (appearance-available-p)
           (not (draw-active-p item)))
    (with-slots (color-list) item
      (unwind-protect
        (progn ;; I don't like this, use theme constant instead!
          (setf color-list (list* :text #.*gray-color* color-list))
          (call-next-method))
        (setf color-list (cddr color-list))))
    (call-next-method)))

#+ccl-5.1 ; code borrowed from MCL 5.1 - perhaps this can also be used for previous versions?
(defmethod view-draw-contents ((item fred-dialog-item))
  (unless (view-quieted-p item)
    (let* (#+ignore (enabled-p (dialog-item-enabled-p item))
           (colorp (color-or-gray-p item)))
      (with-focused-view item
        (with-fore-color (if (and colorp (not (draw-active-p item)))
                           *gray-color*
                           (part-color item :text))
          (unless (part-color item :text)
                        (#_SetThemeTextColor 
             (if (draw-active-p item) #$kThemeTextColorDialogActive #$kThemeTextColorDialogInactive)
             (view-pixel-depth item) (view-color-p item)))
          (with-back-color (part-color item :body)              
            (frec-draw-contents (frec item))
            (#_setorigin :long (view-origin (view-container item)))  ;; << added this for the after method
            (#_SetClip (view-clip-region (view-container item)))))))))

(defmethod view-click-event-handler :before ((item fred-mixin) where)
  ;; Sets key handler immediately as setting it in fred-mixin causes focus ring to appear too late
  (declare (ignore where))
  (when (key-handler-p item)
    (unless (eq item (current-key-handler (view-window item)))
      (set-current-key-handler (view-window item) item nil))))

#|

(make-instance 'dialog
            ; :window-type :movable-dialog
            :window-title "Styling Prefs"
            :view-nick-name 'prefs-dialog
            ;:back-color *tool-back-color*
            :theme-background t
            ;:view-position (dialog-position pm)
            :view-size #@(300 100)
            ;:close-box-p nil
            ; :window-show nil
  :view-subviews
  (list
   (make-dialog-item 'editable-text-dialog-item #@(40 40) #@(40 40) "" nil 
                     :dialog-item-enabled-p t
                     :draw-outline nil
                     :part-color-list `(:frame ,*blue-color* :body ,*red-color*))
   (make-dialog-item 'table-dialog-item #@(100 40) #@(40 40) "" nil :dialog-item-enabled-p nil)
   (make-dialog-item 'fred-dialog-item #@(160 40) #@(40 40) "Test" nil)
))

(select-item-from-list '(a b c d e f g h i j k l m n))

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MAKES SCROLLING FRED VIEW APPEARANCE SAVVY

;; maybe partly covered by scrolling-fred-view-with-frame in MCL 5.1...

(in-package :ccl)

(let ((*warn-if-redefine* nil)
      (*warn-if-redefine-kernel* nil))

#+ignore
(defmethod view-draw-contents :around ((view scrolling-fred-view))
  (if (and (appearance-available-p)
           (not (draw-active-p view))
           (not (typep (view-window view) 'fred-window)))
    (with-fore-color *gray-color*
      (call-next-method))
    (call-next-method)))

#+ignore
(defmethod view-draw-contents :after ((view scrolling-fred-view))  
    (when (draw-scroller-outline view)
      (when (and (appearance-available-p)
                 (not (typep (view-window view) 'fred-window)))
          (let* ((w (view-window view))
                 (active? (window-active-p w))
                 (key-handler? (eq (fred-item view) (current-key-handler w))))
            (with-focused-dialog-item (view)
              (with-item-rect (r view)
                (#_insetrect r 1 1)
                (unless (and active? key-handler?)
                  (#_DrawThemeFocusRect r NIL))
                (#_DrawThemeEditTextFrame r (appearance-theme-state view))
                (when (and active? key-handler?)
                  (#_DrawThemeFocusRect r T))))))))

#+ignore
(defmethod view-draw-contents ((view scrolling-fred-view))
  (progn ;with-focused-view view
    (let ((draw-inactive (and #+carbon-compat (appearance-available-p)
                              (not (draw-active-p view))
                              (not (typep (view-window view) 'fred-window)))))
      (with-fore-color (when draw-inactive *gray-color*)
        #+carbon-compat
        (when draw-inactive
                    (#_SetThemeTextColor #$kThemeTextColorDialogInactive (view-pixel-depth view) (view-color-p view)))
        (if (and (osx-p)(view-get (view-window view) 'theme-background))
          (with-back-color (or (part-color view :body) *white-color*)  ;; kludge to lose the stripes [needed?]
            (call-next-method))
          (call-next-method)))) ; its the one for view - does the subviews    
    (when (draw-scroller-outline view)
      (with-fore-color #+carbon-compat *gray-color* ; bogus, just to restore pen
                       #-carbon-compat (if (or (draw-active-p view)
                                               (not (appearance-available-p)))
                                         *black-color*
                                         *dark-gray-color*)
        #+carbon-compat ; appearance 1.1 required:
        (#_setThemePen
         (if (draw-active-p view) 
           (if (osx-p) #$kThemeBrushButtonFrameActive #$kThemeBrushScrollBarDelimiterActive)
           (if (osx-p) #$kThemeBrushButtonFrameInActive #$kThemeBrushScrollBarDelimiterInactive))
         (view-pixel-depth view)
         (view-color-p view))
        (rlet ((r :rect
                  :topleft 0
                  :bottomright (view-size view))) ; (subtract-points (view-size view) #@(1 1))))
          (#_FrameRect r)))
      (when (and #-carbon-compat (appearance-available-p)
                 (not (typep (view-window view) 'fred-window)))
        (let* ((w (view-window view))
               (active? (window-active-p w))
               (key-handler? (eq (fred-item view) (current-key-handler w))))
          (with-focused-dialog-item (view)
            (with-item-rect (r view)
              (#_insetrect r 1 1)
              (unless (and active? key-handler?)
                (#_DrawThemeFocusRect r NIL))
              (#_DrawThemeEditTextFrame r (appearance-theme-state view))
              (when (and active? key-handler?)
                (#_DrawThemeFocusRect r T)))))))))

(defmethod view-draw-contents ((view scrolling-fred-view))
  (progn ;with-focused-view view
    (let ((draw-inactive (and #+carbon-compat (appearance-available-p)
                              (not (draw-active-p view))
                              (not (typep (view-window view) 'fred-window)))))
      (with-fore-color (when draw-inactive *gray-color*)
        #+carbon-compat
        (when draw-inactive
                    (#_SetThemeTextColor #$kThemeTextColorDialogInactive (view-pixel-depth view) (view-color-p view)))
        (if (and (osx-p)(view-get (view-window view) 'theme-background))
          (with-back-color (or (part-color view :body) *white-color*)  ;; kludge to lose the stripes [still needed?]
            (call-next-method))
          (call-next-method)))) ; its the one for view - does the subviews    
    (when (draw-scroller-outline view)
      (unless (osx-p)
        (with-fore-color #+carbon-compat *gray-color* ; bogus, just to restore pen
                         #-carbon-compat (if (or (draw-active-p view)
                                                 (not (appearance-available-p)))
                                           *black-color*
                                           *dark-gray-color*)
                         #+carbon-compat ; appearance 1.1 required:
                         (#_setThemePen
                          (if (draw-active-p view) 
                            #$kThemeBrushScrollBarDelimiterActive
                            #$kThemeBrushScrollBarDelimiterInactive)
                          (view-pixel-depth view)
                          (view-color-p view))
                         (rlet ((r :rect
                                   :topleft 0
                                   :bottomright (view-size view))) ; (subtract-points (view-size view) #@(1 1))))
                           (#_FrameRect r))))
      (when (and #-carbon-compat (appearance-available-p)
                 (not (typep (view-window view) 'fred-window)))
        (let* ((w (view-window view))
               (active? (window-active-p w))
               (key-handler? (eq (fred-item view) (current-key-handler w))))
          (with-focused-dialog-item (view)
            (with-item-rect (r view)
              (#_insetrect r 1 1)
              (unless (and active? key-handler?)
                (#_DrawThemeFocusRect r NIL))
              (#_DrawThemeEditTextFrame r (appearance-theme-state view))
              (when (and active? key-handler?)
                (#_DrawThemeFocusRect r T)))))))))


) ;; redefine

(defmethod view-corners ((item scrolling-fred-view))
  (if (and #-carbon-compat (appearance-available-p)
           (not (typep item 'CCL::NEW-MINI-BUFFER)))
    (multiple-value-call #'inset-corners (if (osx-p) #@(-3 -3) #@(-2 -2)) (call-next-method))
    (call-next-method)))

(defmethod exit-key-handler :after ((item scrolling-fred-view) new-item)
  (declare (ignore new-item))
  (when (and (appearance-available-p)
             (not (typep item 'CCL::NEW-MINI-BUFFER)))
     (invalidate-view-border item)
     (view-focus-and-draw-contents item)))

(defmethod enter-key-handler :after ((item scrolling-fred-view) old-item)
  (declare (ignore old-item))
  (when (and (appearance-available-p)
             (not (typep item 'CCL::NEW-MINI-BUFFER)))
    ;(invalidate-view-border item)
    (view-focus-and-draw-contents item)))

(defmethod view-activate-event-handler ((item scrolling-fred-view))
  (when (appearance-available-p)
    (invalidate-view item))
  (call-next-method))

(let ((*warn-if-redefine* nil)
      (*warn-if-redefine-kernel* nil))

(defmethod view-deactivate-event-handler ((view scrolling-fred-view))
  (when #+carbon-compat T #-carbon-compat (appearance-available-p)
    ;; # maybe this should be after the call-next-method instead of before? May eliminate blue in scroller...
    (if (osx-p)
      (invalidate-view view)
      (invalidate-view-border view)))
  (if (not (window-active-p (view-window view)))
    (call-next-method)
    (view-deactivate-event-handler (fred-item view))))


) ; end redefine


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MINI BUFFER

;; new-mini-buffer is a subclass of scrolling-fred-view!

(let ((*warn-if-redefine* nil)
      (*warn-if-redefine-kernel* nil))

#+ignore
(defmethod view-draw-contents ((view new-mini-buffer))
  (call-next-method)
  (let ((h-scroller (h-scroller view)))
    (when h-scroller
      (let ((pos (view-position h-scroller)))
        (with-fore-color #-carbon-compat (if (or (not (appearance-available-p))
                                                 (draw-active-p view))
                                           *black-color*
                                           *dark-gray-color*)
                         #+carbon-compat *red-color* ; bogus, just to restore pen
         ;  #+carbon-compat ; Appearance 1.1 required
          (#_setThemePen
           (if (draw-active-p view) 
             (if (osx-p) #$kThemeBrushButtonFrameActive #$kThemeBrushScrollBarDelimiterActive)
             (if (osx-p) #$kThemeBrushButtonFrameInActive #$kThemeBrushScrollBarDelimiterInactive))
           (view-pixel-depth view)
           (view-color-p view))
          (#_moveto :word 0 :word (point-v pos))
          (#_lineto :word (point-h pos) :word (point-v pos)))))))

(defmethod view-draw-contents ((view new-mini-buffer))
  ;(call-next-method)
  (let ((h-scroller (h-scroller view)))
    (when h-scroller
      (let ((pos (view-position h-scroller)))
        (if (osx-p)
          (rlet ((rect :rect 
                       :topleft (make-point 0 (point-v pos)) 
                       :bottomright (make-point (point-h pos) (+ (point-v pos)(point-v (view-size h-scroller))))))
            (#_DrawThemePlacard rect (appearance-theme-state view)))
          (with-fore-color 
            #-carbon-compat 
            (if (or (not (appearance-available-p))
                    (draw-active-p view))
              *black-color*
              *dark-gray-color*)
            #+carbon-compat *red-color* ; bogus, just to restore pen
            #+carbon-compat ; Appearance 1.1 required
            (#_setThemePen
             (if (draw-active-p view) 
               #$kThemeBrushScrollBarDelimiterActive
               #$kThemeBrushScrollBarDelimiterInactive)
             (view-pixel-depth view)
             (view-color-p view))
            (#_moveto :word 0 :word (point-v pos))
            (#_lineto :word (point-h pos) :word (point-v pos))))))
    (unless h-scroller
      (when (osx-p) 
        ; hack... only the frame remains after text has been drawn...
        (rlet ((rect :rect :topleft #@(0 0) :bottomright (view-size view)))
          (#_DrawThemePlacard rect (appearance-theme-state view)))
        )))
  (call-next-method))

(defmethod view-activate-event-handler :before ((item new-mini-buffer))
  (when (appearance-available-p)
    (invalidate-view item)))

(defmethod view-deactivate-event-handler :before ((item new-mini-buffer))
  (when (appearance-available-p)
    (invalidate-view item)))

(defmethod view-draw-contents ((item fred-item))
  (call-next-method)
  (unless (view-quieted-p item)
    (with-focused-view item
      (with-text-colors item
          (frec-draw-contents (frec item))))      
      (#_SetClip (view-clip-region (view-container item)))))


) ; end redefine

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; APPEARANCE & THEME SAVVY TABLE DIALOG ITEM

; This is the URL for how to properly design an OSX scrolling list:
; http://developer.apple.com/documentation/UserExperience/Conceptual/OSXHIGuidelines/XHIGControls/chapter_18_section_6.html#//apple_ref/doc/uid/TP30000359/TPXREF114
; http://developer.apple.com/documentation/mac/HIGuidelines/HIGuidelines-155.html
; http://developer.apple.com/documentation/mac/HIGuidelines/HIGuidelines-140.html
; http://developer.apple.com/documentation/mac/HIGOS8Guide/thig-25.html
; http://developer.apple.com/documentation/Carbon/Reference/Control_Manager/controlman_ref/constant_116.html
; http://developer.apple.com/documentation/LegacyTechnologies/Conceptual/AquaHIGuidelines/AHIGControls/chapter_7_section_2.html


(in-package :ccl)

(let ((*warn-if-redefine* nil)
      (*warn-if-redefine-kernel* nil))



#-carbon-compat
(defmethod view-draw-contents ((item table-dialog-item))
  (without-interrupts
   (let* ((my-dialog (view-container item))
          (wptr (and my-dialog (wptr my-dialog))))
     (when wptr
       (with-focused-dialog-item (item my-dialog)
         (let ((dialog-item-enabled-p (dialog-item-enabled-p item))
               (color-list (part-color-list item))
               (back-color (part-color item :body))
               (pos (view-position item))
               (inner-size (table-inner-size item))
               (appearance? (appearance-available-p))
               (active? (draw-active-p item))
               (key-handler? (eq item (current-key-handler (view-window item)))))
           (rlet ((rect :rect :topleft pos :botright (add-points pos inner-size)))
             (with-clip-rect-intersect rect
               (with-temp-rgns (rgn #+carbon-compat rgn3)
                 (#_getclip rgn)
                 (with-back-color back-color
                   (when back-color
                     (#_erasergn rgn))
                   (when (and *updating* dialog-item-enabled-p)
                     (let ((selection-rgn (if (view-active-p item)
                                            (table-selection-region item)
                                            (table-outline-region item))))
                       (with-hilite-mode
                         (#_InvertRgn selection-rgn))))
                   #-carbon-compat
                   (#_SectRgn rgn (pref wptr :grafport.visrgn) rgn)
                   #+carbon-compat
                   (let ()
                     (get-window-visrgn wptr rgn3)
                     (#_sectrgn rgn rgn3 rgn))
                   (let* ((row (table-top-row item))
                          (column (table-left-column item))
                          (rows (table-rows item))
                          (columns (table-columns item))
                          (first-column column)
                          (cell-size (cell-size item))
                          (column-width (point-h cell-size))
                          (row-height (point-v cell-size))
                          (column-widths-hash (column-widths-hash item))
                          (row-heights-hash (row-heights-hash item))
                          (separator-visible-p (separator-visible-p item))
                          (separator-size (separator-size item))
                          (separator-color (separator-color item))
                          (separator-pattern (separator-pattern item))
                          (might-draw-separator (and separator-visible-p
                                                     (not (eql separator-size #@(0 0)))
                                                     (macptrp separator-pattern)))
                          (draw-col-separator (and might-draw-separator (> columns 1))) ;nil)
                          (top-left (view-position item))
                          (bottom-right (add-points top-left (table-inner-size item)))
                          (top (point-v top-left))
                          (left (point-h top-left))
                          (right (point-h bottom-right))
                          (bottom (point-v bottom-right)))
                     (rlet ((rect :rect :topleft top-left :botright bottom-right))
                       (with-clip-rect-intersect rect
                         (loop
                           (let ((row-height (or (and row-heights-hash (gethash row row-heights-hash)) row-height)))
                             (when (plusp row-height)
                               (setf (pref rect :rect.bottom) (+ (pref rect :rect.top) row-height))
                               (setf (pref rect :rect.left) left)
                               (setq column first-column)
                               #|
                               (when (and might-draw-separator
                                          (or (>= row (1- rows))
                                              (>= (+ (pref rect :rect.bottom) row-height (point-v separator-size)) bottom)))
                                 (setf draw-col-separator t))|#

                               (loop
                                 (let ((column-width (or (and column-widths-hash (gethash column column-widths-hash))
                                                         column-width)))
                                   (setf (pref rect :rect.right) 
                                         (+ (pref rect :rect.left) column-width))
                                   (when (and (plusp column-width)
                                              (#_RectInRgn rect rgn))
                                     (unless (or (>= column columns) (>= row rows))
                                       (when (and appearance? (not active?) (not (osx-p))) 
                                         (setf (slot-value item 'color-list) (list* :text #.*gray-color* color-list)))
                                       (draw-table-cell-new item column row rect (cell-selected-p item column row))
                                       (when (and appearance? (not active?) (not (osx-p)))
                                         (setf (slot-value item 'color-list) (cddr (slot-value item 'color-list))))
                                       (when draw-col-separator
                                         ;; draw the column separator to the right of the current
                                         (with-fore-color separator-color
                                           (with-pen-saved
                                             (#_PenSize :long separator-size)
                                             (#_PenPat separator-pattern)
                                             (#_MoveTo (pref rect :rect.right) top)
                                             (#_LineTo (pref rect :rect.right) (pref rect :rect.bottom)))))))
                                   (incf column)
                                   (when (or (>= column columns)
                                             (>= (incf (pref rect :rect.left) 
                                                       (if (zerop column-width) 
                                                         0 
                                                         (+ column-width (point-h separator-size))))
                                                 right))
                                     (return))))
                               (when (and might-draw-separator (< row rows))
                                 ;; draw the row separator below the current row
                                 (with-fore-color separator-color
                                   (with-pen-saved
                                     (#_PenSize :long separator-size)
                                     (#_PenPat separator-pattern)
                                     (#_MoveTo left (pref rect :rect.bottom))
                                     (#_LineTo (pref rect :rect.right) (pref rect :rect.bottom))))))
                             (incf row)
                             (when (or (>= row rows)
                                       (>= (incf (pref rect :rect.top) 
                                                 (if (zerop row-height) 
                                                   0 
                                                   (+ row-height (point-v separator-size))))
                                           bottom))
                               (return)))))))))))
           (with-item-rect (r item)
             (with-fore-color (getf color-list :frame nil)
               (cond
                 (appearance?
                  (unless (and active? key-handler?)
                    (#_DrawThemeFocusRect r NIL))
                  (#_DrawThemeListBoxFrame r (appearance-theme-state item))
                  (when (and active? key-handler?)
                    (#_DrawThemeFocusRect r T)))
                                  (T
                  (#_insetRect r -1 -1)              
                  (#_FrameRect r))))
             (unless dialog-item-enabled-p
               (rlet ((ps :penstate))
                 (#_GetPenState ps)
                 (#_PenPat *gray-pattern*)
                 (#_PenMode 11)
                 (#_PaintRect r)
                 (#_SetPenState  ps))))))))))

#| Consider this:

(defmacro with-focus-rect ((rect &optional (has-focus T)) &body body)
  `(prog2
     (unless ,has-focus
       (#_DrawThemeFocusRect ,rect NIL))
     (progn ,@body)
     (when ,has-focus
       (#_DrawThemeFocusRect ,rect T))))
|#

#+carbon-compat
(defmethod view-draw-contents ((item table-dialog-item))
  (without-interrupts
   (let* ((my-dialog (view-container item))
          (wptr (and my-dialog (wptr my-dialog))))
     (when wptr
       (with-focused-dialog-item (item my-dialog)
         (let ((dialog-item-enabled-p (dialog-item-enabled-p item))
               #-carbon-compat
               (color-list (part-color-list item))
               (back-color #-carbon-compat (part-color item :body)
                           #+carbon-compat (getf (part-color-list item) :body))
               (pos (view-position item))
               (inner-size (table-inner-size item))
               (appearance? (appearance-available-p))
               (active? (draw-active-p item))
               (key-handler? (eq item (current-key-handler (view-window item)))))
           (rlet ((rect :rect :topleft pos :botright (add-points pos inner-size)))
             (with-clip-rect-intersect rect
               (with-temp-rgns (rgn #+carbon-compat rgn3)
                 (#_getclip rgn)
                 (with-back-color (or back-color #+carbon-compat *red-color*)
                   #+carbon-compat
                   (unless back-color
                     ;; should use a proper value for pix depth!
                    (#_SetThemeBackground #$kThemeBrushListViewBackground 
                     (view-pixel-depth item) (view-color-p item)))
                   (when #-carbon-compat back-color #+carbon-compat t
                     (#_erasergn rgn))
                   (when (and *updating* dialog-item-enabled-p)
                     (let ((selection-rgn (if (view-active-p item)
                                            (table-selection-region item)
                                            (table-outline-region item))))
                       (with-hilite-mode
                         (#_InvertRgn selection-rgn))))
                   #-carbon-compat
                   (#_SectRgn rgn (pref wptr :grafport.visrgn) rgn)
                   #+carbon-compat
                   (let ()
                     (get-window-visrgn wptr rgn3)
                     (#_sectrgn rgn rgn3 rgn))
                   (let* ((row (table-top-row item))
                          (column (table-left-column item))
                          (rows (table-rows item))
                          (columns (table-columns item))
                          (first-column column)
                          (cell-size (cell-size item))
                          (column-width (point-h cell-size))
                          (row-height (point-v cell-size))
                          (column-widths-hash (column-widths-hash item))
                          (row-heights-hash (row-heights-hash item))
                          (separator-visible-p (separator-visible-p item))
                          (separator-size (separator-size item))
                          (separator-color (separator-color item))
                          (separator-pattern (separator-pattern item))
                          (might-draw-separator (and separator-visible-p
                                                     (not (eql separator-size #@(0 0)))
                                                     (macptrp separator-pattern)))
                          (draw-col-separator (and might-draw-separator (> columns 1))) ;nil)
                          (top-left (view-position item))
                          (bottom-right (add-points top-left (table-inner-size item)))
                          (top (point-v top-left))
                          (left (point-h top-left))
                          (right (point-h bottom-right))
                          (bottom (point-v bottom-right)))
                     (rlet ((rect :rect :topleft top-left :botright bottom-right))
                       (with-clip-rect-intersect rect
                         (loop
                           (let ((row-height (or (and row-heights-hash (gethash row row-heights-hash)) row-height)))
                             (when (plusp row-height)
                               (setf (pref rect :rect.bottom) (+ (pref rect :rect.top) row-height))
                               (setf (pref rect :rect.left) left)
                               (setq column first-column)
                               #|
                               (when (and might-draw-separator
                                          (or (>= row (1- rows))
                                              (>= (+ (pref rect :rect.bottom) row-height (point-v separator-size)) bottom)))
                                 (setf draw-col-separator t))|#

                               (loop
                                 (let ((column-width (or (and column-widths-hash (gethash column column-widths-hash))
                                                         column-width)))
                                   (setf (pref rect :rect.right) 
                                         (+ (pref rect :rect.left) column-width))
                                   (when (and (plusp column-width)
                                              (#_RectInRgn rect rgn))
                                     (unless (or (>= column columns) (>= row rows))
                                       ;(when (and appearance? (not active?) #+ccl-5.0 (not (osx-p))) 
                                       ;  (setf (slot-value item 'color-list) (list* :text #.*gray-color* color-list)))
                                       (draw-table-cell-new item column row rect (cell-selected-p item column row))
                                       ;(when (and appearance? (not active?) #+ccl-5.0 (not (osx-p)))
                                       ;  (setf (slot-value item 'color-list) (cddr (slot-value item 'color-list))))
                                       (when draw-col-separator
                                         ;; draw the column separator to the right of the current
                                         (with-fore-color separator-color
                                           (with-pen-saved
                                             (#_PenSize :long separator-size)
                                             (#_PenPat separator-pattern)
                                             (#_MoveTo (pref rect :rect.right) top)
                                             (#_LineTo (pref rect :rect.right) (pref rect :rect.bottom)))))))
                                   (incf column)
                                   (when (or (>= column columns)
                                             (>= (incf (pref rect :rect.left) 
                                                       (if (zerop column-width) 
                                                         0 
                                                         (+ column-width (point-h separator-size))))
                                                 right))
                                     (return))))
                               (when (and #+carbon-compat separator-visible-p #-carbon-compat might-draw-separator (< row rows))
                                 ;; draw the row separator below the current row
                                 (with-fore-color (or separator-color 
                                                      #+carbon-compat *red-color* ;; saves theme drawing state!
                                                      ) 
                                   (with-pen-saved
                                     (cond
                                      #+carbon-compat
                                      ((not separator-color)
                                       (#_setThemePen #$kThemeBrushListViewSeparator 255 t))
                                      (T
                                       (#_PenSize :long separator-size)
                                       (when (macptrp separator-pattern)
                                         (#_PenPat separator-pattern))))
                                     (#_MoveTo left (pref rect :rect.bottom))
                                     (#_LineTo (pref rect :rect.right) (pref rect :rect.bottom))
                                     (incf (pref rect :rect.top) ; quickdraw (pen-size item)
                                           (point-v
                                            (rlet ((pport (:pointer :grafport))
                                                   (pensize :point))
                                              (#_GetPort pport)
                                              (%get-point (#_getportpensize (%get-ptr pport) pensize)))
                                            #+ignore
                                            (let ((wptr (wptr item)))
                                              (with-macptrs ((port (#_getwindowport wptr)))
                                                (rlet ((foo :point))
                                                  (%get-point (#_getportpensize port foo)))))))))))
                             (incf row)
                             (when (or (>= row rows)
                                       (>= (incf (pref rect :rect.top) 
                                                 (if (zerop row-height) 
                                                   0 
                                                   (+ row-height #|(point-v separator-size)|#)))
                                           bottom))
                               (return)))))))))))
           (if appearance?
             (with-item-rect (r item)
               #| #+ccl-5.0
               (when (osx-p)
                 (with-temp-rgns (r-rgn)
                   (#_RectRgn r-rgn r)
                   (decf (pref r :rect.top) 2)
                   (incf (pref r :rect.bottom) 2)
                   (decf (pref r :rect.left) 2)
                   (incf (pref r :rect.right) 1)
                   ;(#_insetRect r 1 -2)
                   (with-temp-rgns (t-rgn) 
                     (#_RectRgn t-rgn r)
                     (#_diffRgn t-rgn r-rgn t-rgn)
                     (with-fore-color *white-color*
                       (#_paintrgn t-rgn))))) |#
               (unless (and active? key-handler?)
                 (#_DrawThemeFocusRect r NIL))
               (#_DrawThemeListBoxFrame r (appearance-theme-state item))
               (when (and active? key-handler?)
                (#_DrawThemeFocusRect r T)))
             #-carbon-compat
             (with-item-rect (r item)
               (with-fore-color (getf color-list :frame nil)                         
                 (#_insetRect r -1 -1)              
                 (#_FrameRect r))
               (unless dialog-item-enabled-p
                 (rlet ((ps :penstate))
                   (#_GetPenState ps)
                   (#_PenPat *gray-pattern*)
                   (#_PenMode 11)
                   (#_PaintRect r)
                   (#_SetPenState  ps)))))))))))

#|
(make-instance 'window
  :theme-background t
  :view-subviews
  (list (make-instance 'sequence-dialog-item :view-position #@(10 10) :view-size #@(100 100) :table-sequence '(abc def ghi))))
|#

#+carbon-compat
(defun %draw-table-cell-new (item h v rect selectedp)
  (when (wptr item)
    (let* ((container (view-container item))
           (enabled-p (dialog-item-enabled-p item))
           (color-p (if (not enabled-p)(color-or-gray-p item))))
      (with-focused-view container
        (let ((cell-fonts (table-cell-fonts item)))
          (multiple-value-bind (ff ms) (view-font-codes item)
            (let* ((top (pref rect rect.top))
                   (key (cons h v))
                   (back-color-p (eq (cell-colors item) :background))
                   (cell-color (part-color-h-v item h v)))
              (declare (dynamic-extent key))
              (without-interrupts
               (let* ((font (and cell-fonts
                                 (gethash key cell-fonts)))
                      (back-color (or (and back-color-p cell-color)
                                      (part-color item :body)))
                      (pos (view-position item))
                      (botright (add-points pos (table-inner-size item))))
                 (with-font-codes
                   (or (car font) ff)
                   (or (cdr font) ms)
                   (with-fore-color (if (or (and (not enabled-p) color-p)
                                            #+ccl-4.3.5
                                            (and (appearance-available-p)
                                                 #+ccl-5.0 (not (osx-p))
                                                 (not (draw-active-p item))) )
                                      *gray-color*
                                      (or (and (not back-color-p) cell-color)
                                          (part-color item :text)
                                          *table-fore-color*
                                          *black-color*))
                  ; # consider using #$kThemeTextColorListView as default color!                    
                     (with-temp-rgns (temp-rgn temp-rgn-2)
                       (#_SetRectRgn temp-rgn (point-h pos) (point-v pos) (point-h botright) (point-v botright))
                       (#_RectRgn temp-rgn-2 rect)
                       (#_sectrgn temp-rgn temp-rgn-2 temp-rgn)
                       ;; mostly for clim?? - with-clip-region does this now
                       ;(#_sectrgn temp-rgn (view-clip-region container) temp-rgn)
                       (with-clip-region temp-rgn
                         (progn ;with-clip-rect-intersect rect
                           (with-back-color back-color
                             #+carbon-compat
                             (cond
                              (selectedp
                               ;; should use a proper value for pix depth!
                               ;; see "Table View Highlighting Styles" at apple's developer connection for further improvements in osx 10.3
                               (#_SetThemeBackground
                                (if (and (window-active-p (view-window item)) (dialog-item-enabled-p item))
                                  #-ccl-5.2 -3 #+ccl-5.2 #$kThemeBrushPrimaryHighlightColor 
                                  #-ccl-5.2 -4 #+ccl-5.2 #$kThemeBrushSecondaryHighlightColor) 
                                (view-pixel-depth item) (view-color-p item)))
                             ((not (getf (part-color-list item) :body))
                               ;; should use a proper value for pix depth!
                               (#_SetThemeBackground #$kThemeBrushListViewBackground 
                                (view-pixel-depth item) (view-color-p item))))
                             (#_eraserect rect))  ;;  change scope -weird?? - from Gilles Bisson                               
                           #+ignore
                           (when (osx-p)
                             (with-fore-color back-color
                               (#_paintrect rect)))
                           (#_moveto  (+ 3 (pref rect rect.left)) (+ top (font-codes-info ff ms)))
                           (draw-cell-contents item h v)
                           #-carbon-compat
                           (when (and selectedp (not *updating*) (dialog-item-enabled-p item))
                             (with-hilite-mode
                               (#_InvertRgn (if (view-active-p item)
                                              (table-selection-region item)
                                              (table-outline-region item)))))))))))))))))))

(defmethod view-corners ((item table-dialog-item))
  (multiple-value-call #'inset-corners 
    (if #+carbon-compat (osx-p) #-carbon-compat NIL
        #@(-7 -9)
        (if (appearance-available-p)      
          #@(-3 -3)
          #@(-1 -1)))
    (call-next-method)))

(defun invert-cell-selection (item h v selected-p)
  (when (wptr item)
    (with-focused-dialog-item (item)
      (with-back-color (or (and (eq (cell-colors item) :background)
                                (part-color-h-v item h v))
                           (part-color item :body))
        (let* ((rgn (if (view-active-p item)
                      (table-selection-region item)
                      (table-outline-region item)))
               (pos (view-position item))
               (botright (add-points pos (table-inner-size item))))
          (with-temp-rgns (temp-rgn)
            (#_SetRectRgn temp-rgn (point-h pos) (point-v pos) (point-h botright) (point-v botright))
            (with-clip-region temp-rgn
              (#_CopyRgn rgn temp-rgn)
              (add-to-selection-region item selected-p h v)
              (#_XorRgn rgn temp-rgn temp-rgn)
              #-carbon-compat
              (with-hilite-mode (#_InvertRgn temp-rgn))
              #+carbon-compat
              (invalidate-region (view-container item) temp-rgn)
              )))))))

#+carbon-compat
(defmethod view-click-event-handler ((item table-dialog-item) where)
  (progn ; without-interrupts
  (let* ((pos (view-position item))
         (botright (add-points pos (table-inner-size item))))
    (if (not (point<= where botright))
      (if (> (point-h where) (point-h botright))
        (let ((vscroll (table-vscroll-bar item)))
          (when vscroll
            (view-click-event-handler vscroll where)))
        (let ((hscroll (table-hscroll-bar item)))
          (when hscroll
            (view-click-event-handler hscroll where))))
      (let* ((type (selection-type item))
             (shift-key-p (shift-key-p))
             (command-key-p (command-key-p))
             (container (view-container item))
             (top-row (table-top-row item))
             (left-column (table-left-column item))
             (rows (table-rows item))
             (bottom-row (+ top-row rows))
             (columns (table-columns item))
             (right-column (+ left-column columns))
             (left (point-h pos))
             (top (point-v pos))
             (right (point-h botright))
             (bottom (point-v botright))
             h v where-h where-v start-selected-p now-in-range last-h last-v)
        (with-focused-dialog-item (item)
          (with-back-color (part-color item :body)
            (#+ccl-5.1 with-timer #-ccl-5.1 progn ; without-interrupts
             (multiple-value-bind (start-h start-v start-in-range) (find-clicked-cell item where)
               (if start-in-range
                 (setq start-selected-p (cell-selected-p item start-h start-v))
                 (deselect-cells item))
               (loop
                (without-interrupts
                 (setq where-h (point-h where)
                       where-v (point-v where))
                 (multiple-value-setq (h v now-in-range) (find-clicked-cell item where))
                 (multiple-value-setq (left-column top-row)
                   (do-auto-scroll item left-column top-row columns rows where-h where-v left top right bottom))
                 (if (and (not now-in-range)(not start-in-range)(not command-key-p)) ;(not shift-key-p))
                   (deselect-cells item)
                   (when (and now-in-range
                              (<= left-column h)
                              (< h right-column)
                              (<= top-row v)
                              (< v bottom-row)
                              (not (and (eql h last-h) (eql v last-v))))
                     (setq last-h h last-v v)
                     (cond ((and (eq type :disjoint)
                                 (or shift-key-p command-key-p)                                 
                                 (eql h start-h)(eql v start-v))
                            (if shift-key-p
                              (cell-select item h v)
                              (if start-selected-p
                                (cell-deselect item h v)
                                (cell-select item h v))))
                           ((and (eq type :disjoint)
                                 command-key-p
                                 start-selected-p)
                            (deselect-cells-between item start-h start-v h v))
                           ((or (eq type :single)
                                (and (not shift-key-p)
                                     (or ;(eq type :contiguous)
                                      (not command-key-p))))
                            (let* ((hash (table-selection-hash item))
                                   (colored-cells-p (colored-cells-p item)))
                              (with-temp-rgns (rgn)
                                (#_SetRectRgn :ptr rgn :long pos :long botright)
                                (with-clip-region rgn
                                  (with-hilite-mode
                                    (if (cell-selected-p item h v)
                                      (if (eq type :single)
                                        (cell-select item h v)
                                        (when hash
                                          (when colored-cells-p
                                            (let ((f #'(lambda (k val)
                                                         (declare (ignore val))
                                                         (unless (and (eql (car k) h)
                                                                      (eql (cdr k) v))
                                                           (cell-deselect item k)))))
                                              (declare (dynamic-extent f))
                                              (maphash f hash)))
                                          (clrhash hash)
                                          (setf (gethash (cons h v) hash) t)
                                          (setf (first-selected-cell-slot item) (make-big-point h v))                                          
                                          (with-temp-rgns (invert-region)
                                            (let ((selection-region
                                                   (if (view-active-p item)
                                                     (table-selection-region item)
                                                     (table-outline-region item))))
                                              (#_CopyRgn selection-region invert-region)
                                              (compute-selection-regions item)
                                              (when (not colored-cells-p)
                                                (#_DiffRgn invert-region selection-region invert-region)
                                                (#_InvertRgn invert-region))
                                              (cell-select item h v)
                                              ))))  ; << fixes bengtsons double click thing                                      (progn
                                      (progn 
                                        (when hash
                                          (when colored-cells-p  ; <<
                                            (deselect-cells item))
                                          (clrhash hash)
                                          (setf (first-selected-cell-slot item) nil)
                                          (when (not colored-cells-p) ; <<
                                            #-carbon-compat
                                            (#_InvertRgn (if (view-active-p item)
                                                           (table-selection-region item)
                                                           (table-outline-region item)))
                                            #+carbon-compat
                                            (invalidate-region (view-container item)
                                                               (if (view-active-p item)
                                                                 (table-selection-region item)
                                                                 (table-outline-region item)))
                                            )
                                          (compute-selection-regions item))
                                        (cell-select item h v))))))))
                           ((and (eq type :contiguous)
                                 command-key-p
                                 (eql h start-h)(eql v start-v))                          
                            (deselect-cells item)
                            (when (not start-selected-p)(cell-select item h v)))
                           ((and (eq type :contiguous)
                                 shift-key-p
                                 (cell-selected-p item h v))
                            (deselect-cells-above item h v))                           
                           (t #|(or (and moved
                                         (or shift-key command-key)
                                         (or contiguous disjoint))
                                    (and contiguous shift-key (not selected)))
                                |#
                              (let* ((p (if (eq type :contiguous)(first-selected-cell item)))
                                     (first-h (if p (point-h p) start-h))
                                     (first-v (if p (point-v p) start-v)))
                                (if (and (eq type :contiguous)  ; don't know bout this
                                         shift-key-p
                                         (neq 1 (point-h (table-dimensions item)))
                                         ;(not (cell-selected-p item h v)) ; always true
                                         )
                                  (multiple-value-bind (max-h max-v)(max-selected-h&v item)
                                    (select-cells-between item
                                                          (min first-h h)
                                                          (min first-v v)
                                                          (max first-h h max-h)
                                                          (max first-v v max-v)))
                                  (select-cells-between item first-h first-v h v))
                                #+ignore
                                (when (and (eq type :contiguous) 
                                           (neq 1 (point-h (table-dimensions item))))
                                  (deselect-cells-above item  h v))))))) )
                                
                 (unless #+ccl-5.1 (wait-mouse-up-or-moved) #-ccl-5.1 (mouse-down-p) (return))
                 (%run-masked-periodic-tasks)
                 (setq where (view-mouse-position container))))))
            (dialog-item-action item)))))))


) ;; redefine

(defmethod view-activate-event-handler :before ((item table-dialog-item))
  (when (appearance-available-p)
    (invalidate-view item #+carbon-compat (osx-p))))

(defmethod view-deactivate-event-handler :before ((item table-dialog-item))
  (when (appearance-available-p)
    (invalidate-view item)))

;; Platinum look & feel of table and sequence dialogs:

#-carbon-compat
(defmethod initialize-instance :around ((item table-dialog-item)
                                    &rest initargs &key part-color-list separator-color separator-size (track-thumb-p T track-thump-p-arg))
  (if (and (appearance-available-p)
           (not (osx-p)))
    (apply #'call-next-method item
     :part-color-list (append part-color-list `(:body ,*lighter-gray-color*))
     :separator-color (or separator-color *white-color*)
     :separator-size (or separator-size #@(1 1))
     :track-thumb-p (if track-thump-p-arg track-thumb-p T)
     initargs)
    (call-next-method)))

#+carbon-compat
(defmethod initialize-instance :around ((item table-dialog-item)
                                    &rest initargs &key part-color-list separator-color separator-size (track-thumb-p T track-thump-p-arg))
  (declare (ignore part-color-list))
  (apply #'call-next-method item 
         :separator-color separator-color ; NIL as default separator color instead of the default initarg
         :track-thumb-p (if track-thump-p-arg track-thumb-p T)
         :separator-size  separator-size ; NIL if none instead of the default initarg
         initargs))

#+carbon-compat
(defmethod separator-size :around ((item table-dialog-item))
  (or (call-next-method)
      (set-slot-value item 'separator-size
       (with-fore-color *red-color* ;; saves theme drawing state!
         (with-pen-saved
           (#_setThemePen #$kThemeBrushListViewSeparator 
            (view-pixel-depth item)(view-color-p item)) 
          ; (let ((wptr (wptr item))) ; quickdraw pen-size...
             ;(if wptr ; cannot eliminate the one for windowport, it causes select-item-from-list etc to malfunction!
             ;  (with-macptrs ((port (#_getwindowport wptr)))
             ;    (rlet ((foo :point))
             ;      (%get-point (#_getportpensize port foo))))
               (rlet ((pport (:pointer :grafport))
                      (pensize :point))
                 (#_GetPort pport)
                 (%get-point (#_getportpensize (%get-ptr pport) pensize))))))))

; borrowed from MCL5 as it is needed when compiling:
#+carbon-compat
(eval-when (:execute :compile-toplevel)
(defmacro do-column-widths ((item column-width &optional (column (gensym))) (&optional start end from-end)
                            &body body)
  (let ((thunk (gensym)))
    `(block nil
       (let ((,thunk #'(lambda (,column-width ,column)
                         (declare (ignore-if-unused ,column))
                         ,@body)))
         (declare (dynamic-extent ,thunk))
         (map-column-widths ,thunk ,item ,start ,end ,from-end)))))

(defmacro do-row-heights ((item row-height &optional (row (gensym))) (&optional start end from-end)
                          &body body)
  (let ((thunk (gensym)))
    `(block nil
       (let ((,thunk #'(lambda (,row-height ,row)
                         (declare (ignore-if-unused ,row))
                         ,@body)))
         (declare (dynamic-extent ,thunk))
         (map-row-heights ,thunk ,item ,start ,end ,from-end)))))
) ; end eval-when

#+ccl-5.0 ; only required for OSX!
(let ((*warn-if-redefine* nil)
      (*warn-if-redefine-kernel* nil))

(defmethod scroll-to-cell ((item table-dialog-item) h &optional v)
  (normalize-h&v h v)
  (let* ((old-top-row (table-top-row item))
         (old-left-column (table-left-column item))
         (rows (table-rows item))
         (columns (table-columns item))
         (visible-end-rows (table-visible-row-count
                            item
                            :end-row rows
                            :from-end t))
         (visible-end-columns (table-visible-column-count
                               item
                               :end-column columns
                               :from-end t))
         (new-top-row (max 0 (min v (- rows visible-end-rows))))
         (new-left-column (max 0 (min h (- columns visible-end-columns))))
         (hscroll (table-hscroll-bar item))
         (vscroll (table-vscroll-bar item))
         (wptr (wptr item)))
    (setf (table-top-row item) new-top-row
          (table-left-column item) new-left-column)
    (when hscroll
      (setf (scroll-bar-setting hscroll) new-left-column))
    (when vscroll
      (setf (scroll-bar-setting vscroll) new-top-row))
    (setf (visible-dimensions-slot item) nil)
    (when wptr
      (with-focused-dialog-item (item)
        (let* ((pos (view-position item))
               (inner-size (table-inner-size item))
               (cell-size (cell-size item))
               (separator-size (separator-size item))
               (cell-size-h (+ (point-h cell-size) (point-h separator-size)))
               (cell-size-v (+ (point-v cell-size) (point-v separator-size)))
               (delta-rows (- old-top-row new-top-row))
               (delta-columns (- old-left-column new-left-column))
               (delta-v 0)
               (delta-h 0))
          (if (row-heights-hash item)
            (cond ((< old-top-row new-top-row)
                   (do-row-heights (item row-height) (old-top-row new-top-row)
                     (decf delta-v row-height)))
                  ((< new-top-row old-top-row)
                   (do-row-heights (item row-height) (new-top-row old-top-row)
                     (incf delta-v row-height))))
            (setq delta-v (* delta-rows cell-size-v)))
          (if (column-widths-hash item)
            (cond ((< old-left-column new-left-column)
                   (do-column-widths (item column-width) (old-left-column new-left-column)
                     (decf delta-h column-width)))
                  ((< new-left-column old-left-column)
                   (do-column-widths (item column-width) (new-left-column old-left-column)
                     (incf delta-h column-width))))
            (setq delta-h (* delta-columns cell-size-h)))
          (rlet ((rect :rect :topleft pos :botright (add-points pos inner-size)))
            (without-interrupts ;; ## only applies to #_scrollrect?
             (let ((container (view-container item)))
               (#-carbon-compat with-macptrs #-carbon-compat ((update-rgn (pref wptr :windowRecord.updateRgn)))
                #+carbon-compat with-temp-rgns #+carbon-compat (update-rgn)
                #+carbon-compat (get-window-updatergn wptr update-rgn)
                 (unless (#_EmptyRgn update-rgn)
                   (let* ((container-origin (subtract-points (view-origin container) (view-position (view-window container)))))
                     (with-temp-rgns (new-update-rgn item-rgn)
                       (#_CopyRgn update-rgn new-update-rgn)
                       (#_CopyRgn (view-clip-region item) item-rgn)
                       ; Work in the container's coordinate system, since we're already focused on it.
                       ; The windowrecord.updatergn is in global coordinates
                       (#_OffsetRgn new-update-rgn (point-h container-origin) (point-v container-origin))
                       (#_OffsetRgn item-rgn (point-h pos) (point-v pos))
                       (#_SectRgn new-update-rgn item-rgn new-update-rgn)
                       (unless (#_EmptyRgn new-update-rgn)
                         (validate-region container new-update-rgn)
                         (#_OffsetRgn new-update-rgn delta-h delta-v)
                         (#_SectRgn new-update-rgn item-rgn new-update-rgn)
                                  (invalidate-region container new-update-rgn))))))
               (with-temp-rgns (invalid-rgn)
                 #+carbon-compat
                 (with-temp-rgns (rgn1 rgn2)
                      (#_RectRgn rgn1 rect)
                   (#_insetRect rect 2 2) ; avoids scrolling traces of the likely blended colors of the sides in OSX
                   (#_RectRgn rgn2 rect)
                   (#_DiffRgn rgn1 rgn2 rgn1)
                   (Invalidate-region container rgn1)) ; invalidates the border area
                 (#_ScrollRect rect delta-h delta-v invalid-rgn)                   
                 (Invalidate-region container invalid-rgn)))))
          ; Could just call compute-selection-regions here, but that makes
          ; scrolling take a long time if there's a large selection.
          ; This code does incremental selection region calculation.
          (let ((selection-region (table-selection-region item))
                (outline-region (table-outline-region item))
                (pos-h (point-h pos))
                (pos-v (point-v pos))
                (inner-size-h (point-h inner-size))
                (inner-size-v (point-v inner-size)))
            (when selection-region
              (#_OffsetRgn selection-region delta-h delta-v)
              (#_OffsetRgn outline-region delta-h delta-v)
              (with-temp-rgns (rgn)
                (#_SetRectRgn rgn
                 (- pos-h cell-size-h)
                 (- pos-v cell-size-v)
                 (+ pos-h inner-size-h cell-size-h)
                 (+ pos-v inner-size-v cell-size-v))
                (#_SectRgn selection-region rgn selection-region)
                (#_SectRgn outline-region rgn outline-region))))
          (let* ((min-column (1- (table-left-column item)))
                 (left-column (table-left-column item))
                 (visible-columns (table-visible-column-count item :start-column left-column :end-column columns))
                 (max-column (+ min-column visible-columns 2))
                 (top-row (table-top-row item))
                 (visible-rows (table-visible-row-count item :start-row top-row :end-row rows))
                 (min-row (1- (table-top-row item)))
                 (max-row (+ min-row visible-rows 2)))
            (if (< delta-rows 0)
              (setq min-row (+ max-row delta-rows))
              (setq max-row (+ min-row delta-rows)))
            (if (< delta-columns 0)
              (setq min-column (+ max-column delta-columns))
              (setq max-column (+ min-column delta-columns)))
            (compute-selection-regions item min-row max-row min-column max-column)))))))

) ; end redefine
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PATCHES THE ARROW DIALOG ITEM FOR APPEARANCE

(in-package :ccl)

;; require appr-table-dialog-item...


(let ((*warn-if-redefine* nil)
      (*warn-if-redefine-kernel* nil))

(defun frame-table-item (item &optional pattern #+ccl-5.1 (inset -4))
  (declare (ignore pattern inset))
  (let* ((w (view-window item)))
    (if (appearance-available-p)
      (with-back-color (slot-value w 'back-color) 
        (with-item-rect (r item)
          ;; Moved drawing of frame to table-dialog-item so that it is always covered.
          ;(#_DrawThemeFocusRect r
          ;  (and (window-active-p w)(eq item (current-key-handler w))))
          ;(unless (eq item (current-key-handler w))
          ;  (#_DrawThemeListBoxFrame r 
          ;    (if (window-active-p w) #$kThemeStateActive #$kThemeStateDisabled)))
          ))
      #-carbon-compat
      (when (and w (cdr (key-handler-list w)))      
       (let ((pos (view-position item)))
        (rlet ((rect :rect topleft pos bottomright (add-points pos (view-size item))))
          (#_insetrect :ptr rect :long #@(-4 -4))
          (if (and (window-active-p w)(eq item (current-key-handler w)))
            (rlet ((ps :penstate))
              (#_GetPenState :ptr ps)
              (#_PenPat :ptr *black-pattern*)
              (#_framerect :ptr rect)
              (#_insetrect :ptr rect :long #@(1 1))
              (#_framerect :ptr rect)
              (#_SetPenState :ptr ps))
            (let ((rgn *temp-rgn*)    ; <<
                  (rgn2 *temp-rgn-2*))
              (#_rectrgn rgn rect)
              (#_insetrect :ptr rect :long #@(3 3))
              (#_rectrgn rgn2 rect)
              (#_diffrgn rgn rgn2 rgn)
              (with-back-color (slot-value w 'back-color)  ; why needed?
                (#_erasergn rgn)
                )))))))))

#-ccl-5.1 ; no difference in MCL 5.1b2
(defmethod view-draw-contents ((item arrow-dialog-item))
  (call-next-method)
  (let ((w (view-window item)))
    (when  w
      (frame-table-item item))))

(defmethod enter-key-handler :after ((item arrow-dialog-item) new-item)
  (declare (ignore new-item))
  (when (appearance-available-p)
     ; (invalidate-view-border item)
     (view-focus-and-draw-contents item)))

(defmethod exit-key-handler :after ((item arrow-dialog-item) new-item)
  (declare (ignore new-item))
  (when (appearance-available-p)
     ; (invalidate-view-border item)
     (view-focus-and-draw-contents item)))

(defmethod view-corners ((item arrow-dialog-item))
  (if (appearance-available-p)
    (call-next-method)
    #-carbon-compat
    (let ((pos (view-position item)))
      (values
        (subtract-points pos #@(4 4))
        (add-points pos (add-points (view-size item) #@(4 4)))))))

)  ;; end redefine

; (select-item-from-list '(a b c d e f g h i j k l))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LEFT BORDER VIEW (used in Apropos dialog of MCL)

(let ((*warn-if-redefine* nil)
      (*warn-if-redefine-kernel* nil))

(defmethod view-draw-contents ((v left-border-view))
  (let (#-carbon-compat (color-p (color-or-gray-p (view-window v))))
    (call-next-method)
    (if (appearance-available-p)
      (rlet ((rect :rect))
        (rset rect :rect.topleft #@(0 0))
        (rset rect :rect.bottomright (make-point 3 (1- (point-v (view-size v)))))
        (#_DrawThemeSeparator rect (appearance-theme-state v)))
      #-carbon-compat
      (progn
        (#_MoveTo 0 0)
        (with-fore-color (if color-p *white-color* *black-color*)
          (#_LineTo 0 (1- (point-v (view-size v)))))
        (when color-p
          (#_moveto 1 0)
          (with-fore-color *tool-line-color*
            (#_LineTo 1 (1- (point-v (view-size v))))))))))
)

(defmethod view-activate-event-handler :before ((view left-border-view))
  (when (appearance-available-p)
    (invalidate-corners view #@(0 0) (make-point 3 (1- (point-v (view-size view)))) #+ccl-5.0 (osx-p))))

(defmethod view-deactivate-event-handler :before ((view left-border-view))
  (when (appearance-available-p)
    (invalidate-corners view #@(0 0) (make-point 3 (1- (point-v (view-size view)))) #+ccl-5.0 (osx-p))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UNDERLINED VIEW (used in Trace dialog of MCL)


(let ((*warn-if-redefine* nil)
      (*warn-if-redefine-kernel* nil))

#-carbon-compat
(defmethod view-draw-contents ((item underlined-view))  
  (let* ((size (subtract-points (view-size item) #@(1 1)))   ; allow for descenders not to smash into line
         (topleft (view-position item))
         (bottomright (add-points topleft size))
         (bottomleft (add-points topleft (make-point 0 (point-v size))))
         (color-p (color-or-gray-p (view-window item)))
         (appearance? (appearance-available-p))
         (enabled? (or (null appearance?) (draw-active-p item))))
    (multiple-value-bind (ff ms)(view-font-codes item)
      (let ((ascent (font-codes-info ff ms)))
        (with-font-codes ff ms
         (with-back-color (if (and (osx-p)
                                    (view-get (view-window item) 'theme-background))
                             *white-color* 
                             nil)
          (with-pstrs ((p-title (dialog-item-text item)))      
            (#_MoveTo (point-h topleft)
             (+ (point-v topleft) ascent 1))
            (with-fore-color (if (or enabled? (osx-p)) *black-color* *gray-color*)
              (#_Drawstring :ptr p-title))
            (if appearance?
              (rlet ((rect :rect))
                (rset rect :rect.topleft (- bottomleft #@(0 2)))
                (rset rect :rect.bottomright bottomright)
                (#_DrawThemeSeparator rect (appearance-theme-state item)))
              (progn
                (#_MoveTo :long bottomleft)
                (if color-p
                  (with-fore-color *tool-line-color*      
                    (#_LineTo :long bottomright))
                  (#_lineto :long bottomright))
                (#_MoveTo :long (subtract-points bottomleft #@(0 1)))
                (with-fore-color (if color-p *white-color* *black-color*)
                  (#_LineTo :long (subtract-points bottomright #@(0 1)))))))))))))

#+carbon-compat
(defmethod view-draw-contents ((item underlined-view))  
  (let* ((size (subtract-points (view-size item) #@(1 1)))   ; allow for descenders not to smash into line
         (topleft (view-position item))
         (bottomright (add-points topleft size))
         (bottomleft (add-points topleft (make-point 0 (point-v size))))
         (theme-state (if (draw-active-p item) #$kThemeStateActive #$kThemeStateInactive)))
    (multiple-value-bind (ff ms)(view-font-codes item)
      (with-font-codes ff ms
        (with-back-color (if (view-get (view-window item) 'theme-background) *white-color* nil)
          (with-fore-color *red-color* ; bogus to restore pen
                        (#_SetThemeTextColor 
             (if (draw-active-p item) #$kThemeTextColorDialogActive #$kThemeTextColorDialogInactive)
             (view-pixel-depth item)
             (view-color-p item))
            (rlet ((rect :rect :topleft (add-points topleft #@(0 1)) :bottomright bottomright))
              (with-cfstrs ((cftext (dialog-item-text item) )) ; use draw-theme-text-box instead?
                (#_Drawthemetextbox cftext #$kThemeCurrentPortFont theme-state t rect #$teFlushDefault *null-ptr*)))
            (rlet ((rect :rect :topleft (subtract-points bottomleft #@(0 2)) :bottomright bottomright))
              (#_DrawThemeSeparator rect theme-state))))))))

)

(defmethod view-activate-event-handler :before ((item underlined-view))
  #-ccl-5.0
  (when (appearance-available-p)
    (invalidate-view item #+ccl-4.3.5 T))
  #+ccl-5.0
  (invalidate-view item t))

(defmethod view-deactivate-event-handler :before ((item underlined-view))
  #-ccl-5.0
  (when (appearance-available-p)
    (invalidate-view item #+ccl-4.3.5 T))
  #+ccl-5.0
  (invalidate-view item t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MAKE TITLE BOX DIALOG ITEMS APPEARANCE SAVVY

;; The title box dialog item is in an optional library module used by the MCL Interface Toolkit.
;; I recommend renaming it to group-box-dialog-item in preparation for making a more
;; generalized view for grouping subviews.

(when (find-class 'title-box-dialog-item NIL)

(defmethod view-draw-contents ((item title-box-dialog-item))
  (let* ((topleft (view-position item))
         (bottomright (add-points topleft (view-size item)))
         (text (dialog-item-text item)))
    (multiple-value-bind (offset top-offset descent) (label-offset item)
      (rlet ((r :rect 
                :topleft (add-points topleft (make-point (point-h offset) top-offset))
                :bottomright (add-points topleft
                                         (make-point (+ (point-h offset) (title-box-width item) 4)
                                                     (+ (point-v offset) descent)))))
            ; (#_EraseRect :ptr r)
            (rlet ((frame :rect :topleft topleft
                          :bottomright bottomright))
              (with-temp-rgns (visible-rgn)
                (#_rectRgn visible-rgn frame)
                (when text
                  (with-temp-rgns (title-rgn)
                    (#_rectRgn title-rgn r) 
                    (#_diffRgn visible-rgn title-rgn visible-rgn)))
                (with-clip-region visible-rgn
                  (if #-carbon-compat (appearance-available-p) #+carbon-compat T
                    (#_DrawThemePrimaryGroup frame (appearance-theme-state item))
                    #-carbon-compat
                    (#_FrameRect :ptr frame)))))
            (when text
              (with-fore-color (if (draw-active-p item) *black-color* *gray-color*)                
                #+carbon-compat
                (with-cfstrs ((cftext text)) 
                                    (#_SetThemeTextColor  ;; available with appearance 1.0
                   (if (draw-active-p item) #$kThemeTextColorDialogActive #$kThemeTextColorDialogInactive)
                   (view-pixel-depth view)
                   (view-color-p view))
                  (#_Drawthemetextbox ; carbonLib 1.3/OSX - use draw-theme-text-box instead?
                   cftext #$kThemeCurrentPortFont 
                   (appearance-theme-state item) t r #$tejustcenter *null-ptr*))
                #-carbon-compat
                #|(rlet ((r :rect :topleft topleft
                        :bottomright bottomright))          
                (rset r rect.left (+ (rref r rect.left) 4))
                (rset r rect.bottom (+ (rref r rect.top) 2))
                (rset r rect.right (+ (rref r rect.left) 4 (title-box-width item)))
                (#_EraseRect :ptr r) |#
                (with-pstrs ((p-title text))
                  (#_MoveTo :long (add-points topleft (label-offset item)))
                  (#_DrawString :ptr p-title))))))))

#+carbon-compat
(defun label-offset (title-box-dialog-item)
  "Returns three values: offset, top-vertical-offset and descent"
  (multiple-value-bind (ff ms) (view-font-codes title-box-dialog-item) ; consider to use with-font-focused-view
    (with-font-codes ff ms
      (rlet ((size :point)
             (baseline :signed-word))
        (with-cfstrs ((cftext (or (dialog-item-text title-box-dialog-item) ""))) 
          (#_GetThemeTextDimensions cftext 
           #$kThemeCurrentPortFont 
           (appearance-theme-state title-box-dialog-item)
           NIL
           size
           baseline))
        (let* ((baseline (%get-signed-word baseline))
               (size (%get-point size))
               (ascent (+ (point-v size) baseline))
               (descent (abs baseline))
               (ascent/2 (floor ascent 2)))
          (values (make-point (if (osx-p) 11 6) (if (osx-p) (- -3 descent) ascent/2))
                  (if (osx-p) (- -3 ascent descent) (- ascent/2 ascent))
                  descent))))))

(defun update-title-box-width (item)
  (when (wptr item)
    (multiple-value-bind (ff ms) (view-font-codes item)
    (setf (title-box-width item)
      (if (dialog-item-text item)
        (with-font-codes ff ms ; consider to use with-font-focused-view
          #+carbon-compat ; should be a function!
          (rlet ((size :point)
                 (baseline :signed-word))
            (with-cfstrs ((cftext (dialog-item-text item))) 
              (#_GetThemeTextDimensions cftext 
               #$kThemeCurrentPortFont 
               (appearance-theme-state item)
               NIL
               size
               baseline))
            (point-h (%get-point size)))
          #-carbon-compat
          (string-width (dialog-item-text item)))
        0)))))

#+carbon-compat
(defmethod view-default-font ((view title-box-dialog-item))
  (sys-font-spec))

(defmethod view-activate-event-handler :before ((item title-box-dialog-item))
  #-ccl-5.0
  (when (and (appearance-available-p)
             (not (osx-p)))
    (invalidate-view item))
  #+ccl-5.0
  (invalidate-view item (osx-p)))

(defmethod view-deactivate-event-handler :before ((item title-box-dialog-item))
  #-ccl-5.0
  (when (and (appearance-available-p)
             (not (osx-p)))
    (invalidate-view item))
  #+ccl-5.0
  (invalidate-view item (osx-p)))

) ; end title-box-dialog-item redefinition

#|
(make-instance 'window
  :theme-background T
  :view-subviews
    (list
      (make-dialog-item 'title-box-dialog-item #@(10 10) #@(300 16) "Hello")))

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SCROLL BAR

(let ((*warn-if-redefine* nil)
      (*warn-if-redefine-kernel* nil))

(defmethod view-draw-contents ((item scroll-bar-dialog-item))
  (let* ((handle (dialog-item-handle item))
         #-carbon-compat
         (window (view-window item)))
    (when handle
      (if #+carbon-compat T
          #-carbon-compat (or (appearance-available-p)
                              (window-active-p window))
        (if #-carbon-compat (neq 0 (href handle controlRecord.contrlvis))
            #+carbon-compat (#_iscontrolvisible handle)
            (#_Draw1Control handle)
            (#_ShowControl handle))
       #-carbon-compat
        (multiple-value-bind (tl br) (scroll-bar-and-splitter-corners item)          
          (rlet ((rect :rect :topLeft tl :botRight br))
            (let* ((bc (get-back-color window)))
              ;(when (not (eql bc *white-color*))
                (with-back-color bc
                 (#_eraserect rect)));)
            (with-fore-color *dark-gray-color*
              (#_FrameRect rect))))))))

(defmethod view-activate-event-handler ((item scroll-bar-dialog-item))
  (when (draw-active-p item)
    (let ((handle (dialog-item-handle item))
          (container (view-container item)))
      (with-focused-view container
        (when (dialog-item-enabled-p item)
          (if (#+carbon-compat appearance-available-p)
            (#_ActivateControl handle) ;; recommended by apple to replace hilitecontrol
            (#_hilitecontrol handle 0))
          #-carbon-compat
          (let ((splitter (pane-splitter item)))
            (when splitter (view-draw-contents splitter)))
          )
        (unless #-carbon-compat (neq 0 (rref handle :controlRecord.ContrlVis))
                #+carbon-compat (#_iscontrolvisible handle)
          ; #_ShowControl is similarly naughty
          (let* ((wptr #-carbon-compat (href handle :controlRecord.ContrlOwner)
                       #+carbon-compat (#_getcontrolowner handle))
                 #-carbon-compat
                 (update-rgn (pref wptr :windowRecord.updateRgn))
                 #+carbon-compat
                 (update-rgn *temp-rgn-3*)
                 (temp-rgn *temp-rgn*))
            (declare (dynamic-extent wptr update-rgn)
                     (type macptr wptr update-rgn))
            #-carbon-compat
            (progn
              (#_CopyRgn update-rgn temp-rgn)
              (#_ShowControl handle)
              (#_CopyRgn temp-rgn update-rgn))
            #+carbon-compat
            (progn
              (get-window-updatergn wptr update-rgn)
              (#_showcontrol handle)
              (get-window-updatergn wptr temp-rgn)
              (valid-window-rgn wptr temp-rgn)
              (inval-window-rgn wptr update-rgn)))
            
          (let ((splitter (pane-splitter item)))
            (when splitter (view-draw-contents splitter)))
          (multiple-value-bind (tl br) (scroll-bar-and-splitter-corners item)
            (validate-corners container tl br))))))
;  (call-next-method)
)

(defmethod view-deactivate-event-handler ((item scroll-bar-dialog-item))
  (let ((handle (dialog-item-handle item))
        (container (view-container item)))
    (when handle
      (with-focused-view container
        (unless (draw-active-p item)
          #+carbon-compat
          (when (appearance-available-p)
            (#_DeactivateControl handle)) ;; recommended by apple to replace hilitecontrol
          (if (appearance-available-p)
            (call-next-method)
            (multiple-value-bind (tl br)(scroll-bar-and-splitter-corners item)
              (rlet ((rect :rect
                           :topLeft (add-points tl #@(1 1))
                           :botRight (subtract-points br #@(1 1))))
                (with-clip-rect-intersect rect
                  ; #_HideControl invals outside of the clip rect.  Naughty, naughty.
                  (let* ((wptr #-carbon-compat (href handle :controlRecord.ContrlOwner)
                               #+carbon-compat (#_getcontrolowner handle))
                         #-carbon-compat
                         (update-rgn (pref wptr :windowRecord.updateRgn))
                         #+carbon-compat
                         (update-rgn *temp-rgn-3*)
                         (temp-rgn *temp-rgn*))
                    
                    (declare (dynamic-extent wptr update-rgn)
                             (type macptr wptr update-rgn))
                    #-carbon-compat
                    (progn
                      (#_CopyRgn update-rgn temp-rgn)
                      ;(#_HideControl handle)
                      (#_CopyRgn temp-rgn update-rgn))
                    #+carbon-compat
                    (progn
                      (get-window-updatergn wptr update-rgn)
                      (#_hidecontrol handle)
                      (get-window-updatergn wptr temp-rgn)
                      (valid-window-rgn wptr temp-rgn)
                      (inval-window-rgn wptr update-rgn)))
                  (with-back-color (get-back-color (view-window item))
                    (#_EraseRect rect))
                  (validate-corners container tl br)))))))
      (unless (#+carbon-compat appearance-available-p)
        (#_hilitecontrol handle 255)))))


 ) ;end redefine

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FRED PANE SPLITTER

;; The black rectangle look of the Fred pane splitters was suggested by the User Interface Guidelines
;; for previous versions of the MacOS, but doesn't fit as well into Platinum/Aqua. The following implements
;; a pane splitter using embossed lines similar to what is used in the Finder to signify an area for dragging/extending.

;; Consider to use #$kThemeMetricPaneSplitterHeight for the default size of the pane splitter...

(defun draw-dragger (rect direction &optional (active t)) ; surely there is a better name for this function...
  "Draw a suitable graphics to signify that something can be dragged in the given direction"
  (with-fore-color *red-color* ; just to restore pen
    (let ((highlight (if active
                       #$kThemeBrushButtonActiveDarkHighlight 
                       #$kThemeBrushButtonInactiveDarkHighlight
                       ))
          (shadow (if active
                    #$kThemeBrushBevelActiveDark ; #$kThemeBrushButtonActiveDarkShadow
                    #$kThemeBrushBevelinActiveDark ; #$kThemeBrushButtonInactiveDarkShadow
                    )))
      (ecase direction
        (:vertical
         (do ((left (pref rect :rect.left))
              (right (pref rect :rect.right))
              (top (pref rect :rect.top) (+ top 2)))
             ((>= top (pref rect :rect.bottom)))
           (#_setThemePen highlight 256 t)
           (#_MoveTo left top)
           (#_LineTo right top)             
           (#_setThemePen shadow 256 t)
           (#_MoveTo (1+ left) (1+ top))
           (#_LineTo (1+ right) (1+ top))))
        (:horizontal
         (do ((top (pref rect :rect.top))
              (bottom (pref rect :rect.bottom))
              (left (pref rect :rect.left) (+ left 2)))
             ((>= left (pref rect :rect.right)))
           (#_setThemePen highlight 256 t)
           (#_MoveTo left top)
           (#_LineTo left bottom)             
           (#_setThemePen shadow 256 t)
           (#_MoveTo (1+ left) (1+ top))
           (#_LineTo (1+ left) (1+ bottom))))))))

#| Alternative way of drawing the dragger using theme arrows: 
    (ecase direction
      (:vertical
       (let ((middle (floor (+ (pref rect :rect.top) (pref rect :rect.bottom)) 2))
             (split (floor (+ (pref rect :rect.left) (pref rect :rect.right)) 2)))
         (rlet ((toprect :rect 
                         :top (- middle 5)
                              :left (- split 2)
                         :right (+ split 6)
                         :bottom (+ middle 1))
                (botrect :rect
                         :top (+ middle 2)
                         :left (- split 2)
                         :right (+ split 6)
                             :bottom (+ middle 9)))
             (#_DrawThemePopupArrow toprect #$kThemeArrowUp #$kThemeArrow7pt 
              (if active #$kThemeStateActive #$kThemeStateInactive) (%null-ptr) 0)
             (#_DrawThemePopupArrow botrect #$kThemeArrowDown #$kThemeArrow7pt 
              (if active #$kThemeStateActive #$kThemeStateInactive) (%null-ptr) 0)))))))
|#

(let ((*warn-if-redefine* nil)
      (*warn-if-redefine-kernel* nil))

(defclass pane-splitter (simple-view)
  ((scrollee :initarg :scrollee 
             :reader scroll-bar-scrollee)
   (direction :initarg :direction :reader scroll-bar-direction)
   (cursor :initarg :cursor :initform :arrow-cursor :accessor pane-splitter-cursor)
   (scroll-bar :initarg :scroll-bar :initform nil :reader scroll-bar)))

#| Pane splitter with icon to symbolize a split pane:

(defmethod view-draw-contents ((item pane-splitter))
  (if (#+carbon-compat osx-p)
    #-carbon-compat NIL
    #+carbon-compat ; appearance 1.1 required
    (with-item-rect (rect item)
      (when (osx-p)
         (ecase (scroll-bar-direction item)
           (:horizontal
            (decf (pref rect :rect.bottom)))
           (:vertical
            (decf (pref rect :rect.right)))))
      (with-back-color *red-color* ; just to restore background
        (#_DrawThemePlacard rect (appearance-theme-state item));; just to get the border...
        (#_SetThemeBackground 
         (if (draw-active-p item)
           #$kThemeBrushBevelActiveLight ; #$kThemeBrushButtonFaceActive might be better...
           #$kThemeBrushBevelInactiveLight ; #$kThemeBrushButtonFaceInactive might be better...
           )
         (view-pixel-depth item) (view-color-p item))
        (#_insetRect rect 1 1)
        (#_EraseRect rect)
        (#_insetRect rect -1 -1)
        (; with-pen-saved
         with-fore-color *black-color* ; bogus, just to get it restored
         (unless (osx-p) ;; it didn't come out well in osx...
           (#_setThemePen
            (if (draw-active-p item)
              (if (osx-p) #$kThemeBrushButtonFrameActive #$kThemeBrushScrollBarDelimiterActive)
              (if (osx-p) #$kThemeBrushButtonFrameInactive #$kThemeBrushScrollBarDelimiterInactive))
            256 t) ;; ## need to fix the depth here...
           (#_MoveTo (pref rect :rect.left) (pref rect :rect.top))
           (ecase (scroll-bar-direction item)
             (:vertical
              (#_LineTo (pref rect :rect.left) (pref rect :rect.bottom)))
             (:horizontal
              (#_LineTo (pref rect :rect.right) (pref rect :rect.top)))))
                  (#_SetThemeTextColor (if (window-active-p (view-window item))
                                                              #$kThemeTextColorPlacardActive 
                                                              #$kThemeTextColorPlacardInactive)
                    (view-pixel-depth item) (view-color-p item))
                  (let ((left (pref rect :rect.left))
              (top (pref rect :rect.top))
              (right (pref rect :rect.right))
              (bottom (pref rect :rect.bottom)))
           (ecase (scroll-bar-direction item)
             (:vertical
              (#_MoveTo (+ left 3) (+ top 2))
              (#_LineTo (+ left 3) (- bottom 3))
              (#_MoveTo (- right 4) (+ top 2))
              (#_LineTo (- right 4) (- bottom 3))
              (#_MoveTo (+ left 3) (floor (+ top bottom) 2))
              (#_LineTo (- right 4) (floor (+ top bottom) 2)))
             (:horizontal
              (#_MoveTo (+ left 2) (+ top 3))
              (#_LineTo (- right 3) (+ top 3))
              (#_MoveTo (+ left 2) (- bottom 4))
              (#_LineTo (- right 3) (- bottom 4))
              (#_MoveTo (floor (+ left right) 2) (+ top 3))
              (#_LineTo (floor (+ left right) 2) (- bottom 4))))))))
    (if (draw-active-p item)
      ;(let* ((tl (view-position item))
      ;       (br (add-points tl (view-size item))))
      ;  (rlet ((r :rect :topleft tl :botright br))
      (with-item-rect (r item)
        (with-fore-color *black-color*
          (#_FillRect r *black-pattern*)))
      (with-fore-color *gray-color*
        #+carbon-compat ; Appearance 1.1 required
        (#_setThemePen #$kThemeBrushScrollBarDelimiterInactive 
         (view-pixel-depth item) (view-color-p item))
        (with-item-rect (r item)
          (#_FillRect r *black-pattern*))))))
|#

; A pane splitter with Aqua colors:

(defmethod view-draw-contents ((item pane-splitter))
  (with-fore-color (if (draw-active-p item) *black-color* *gray-color*)
    (with-item-rect (r item)
      #+carbon-compat
      (when (osx-p)
         (ecase (scroll-bar-direction item)
           (:horizontal
            (decf (pref r :rect.bottom)))
           (:vertical
            (decf (pref r :rect.right)))))
      #+(and carbon-compat (not alice)) ; Appearance 1.1 required
      (#_setThemePen 
       (if (osx-p)
         (if (draw-active-p item) #$kThemeBrushFocusHighlight #$kThemeBrushBevelInactiveLight)
         (if (draw-active-p item) #$kThemeBrushBlack #$kThemeBrushScrollBarDelimiterInactive))
      (view-pixel-depth item) (view-color-p item))
      (#_FillRect r *black-pattern*))))

(defmethod view-deactivate-event-handler ((item pane-splitter))
  (invalidate-view item))

(defmethod view-activate-event-handler ((item pane-splitter))
  (invalidate-view item))


) ; end redefine

(defvar *old-left-ps-cursor* *left-ps-cursor*)
#+carbon-compat (setf *left-ps-cursor* :resize-right-cursor)
(defvar *old-right-ps-cursor* *right-ps-cursor*)
#+carbon-compat (setf *right-ps-cursor* :resize-left-cursor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3D BUTTON

(let ((*warn-if-redefine* nil)
      (*warn-if-redefine-kernel* nil))

(defun draw-up-rect (top-left bottom-right highlight-color shadow-color)
  (let* ((h1 (point-h top-left))
         (v1 (point-v top-left))
         (h2 (point-h bottom-right))
         (v2 (point-v bottom-right)))
    (with-fore-color (if (numberp highlight-color) 
                       highlight-color 
                       (or #-carbon-compat 
                           (when (search "PRESSED" (string highlight-color)) *tool-line-color*)
                           *white-color*))
      (when (keywordp highlight-color)
        (#_setThemePen (cdr (assoc highlight-color *theme-brushes-alist*)) 256 t)) ; need better depth!
      (#_moveto  h1 v1)
      (#_lineto  h1 v2)
      (with-fore-color (if (numberp shadow-color) 
                         shadow-color 
                         (or #-carbon-compat
                             (when (search "PRESSED" (string shadow-color)) *white-color*)
                             *tool-line-color*))
        (when (keywordp shadow-color)
          (#_setThemePen (cdr (assoc shadow-color *theme-brushes-alist*)) 256 t)) ; need better depth!
        (#_moveto  h2 v1)
        (#_lineto  h2 v2)
        (#_lineto  h1 v2))
      ; (with-fore-color light-color
      (#_moveto  h1 v1)
      (#_lineto  h2 v1))))

(defclass 3d-button (default-button-mixin dialog-item)
  ((pushed-state :initform nil :accessor pushed-state)
   (frame-p :initform nil :initarg :frame-p :accessor frame-p))
  #+carbon-compat
  (:default-initargs
    :view-font :small-system-font)
  #-ccl-4.3.1
  (:default-initargs
    :part-color-list `(:back-color ,*tool-back-color* :dark-color ,*tool-line-color*)))

(defmethod dark-color ((b 3d-button))
  (or (part-color b :dark-color) 
      (if (draw-active-p b)
        (if (pushed-state b)
          :Button-pressed-Dark-Highlight-brush
          :Button-Active-Dark-Shadow-brush)
        :Button-inactive-Dark-Shadow-brush)))

(defmethod light-color ((b 3d-button))
  (or (part-color b :light-color)
      (if (draw-active-p b)
        (if (pushed-state b)
          :button-pressed-dark-shadow-brush
          :Button-Active-Dark-Highlight-Brush)
        :Button-inactive-Dark-Highlight-Brush)))

(defmethod lighter-color ((b 3d-button))
  (or (part-color b :lighter-color) 
      #-ccl-4.3.1 (light-color b)
      #+ccl-4.3.1 (if (draw-active-p b)
                        (if (pushed-state b)
                          :button-pressed-light-shadow-brush
                          :Button-Active-Light-Highlight-brush)
                        :Button-inactive-Light-Highlight-brush)))

(defmethod darker-color ((b 3d-button))
  (or (part-color b :darker-color) 
      #-ccl-4.3.1 #x777777
      #+ccl-4.3.1 (if (draw-active-p b)
                        (if (pushed-state b)
                          :button-pressed-light-highlight-brush
                          :Button-Active-Light-shadow-brush)
                     :Button-inactive-Light-shadow-brush)))

(defmethod frame-color ((b 3d-button))
  (or (part-color b :frame) 
      #-ccl-4.3.1 *black-color*
      #+ccl-4.3.1
      (if (draw-active-p b)
        :button-frame-active-brush
        :button-frame-inactive-brush)))

(defmethod button-color ((b 3d-button))
  (or (part-color b :back-color)
      #+ccl-4.3.1
      (if (draw-active-p b)
        (if (pushed-state b)
          :button-face-pressed-brush
          :button-face-active-brush) 
        :button-face-inactive-brush)))

(defmethod view-draw-contents :before ((item 3d-button))
  "erase area with appropriate color"
  (when (and (view-container item) (view-window item))
    (with-focused-view item
      (rlet ((rect :rect :topleft #@(1 1) 
                   :botright (subtract-points (view-size item) #@(1 1))))
        (let ((back-color (if (and (dialog-item-enabled-p item)
                                   (default-button-p item))
                            (or (button-default-color item) (darker-color item))
                            (or (button-color item) (get-back-color (view-window item))))))
          (with-back-color (if (keywordp back-color)
                             #+carbon-compat *red-color* ; bogus, just to restore back color
                             #-carbon-compat ; required color as theme background may not work.
                             (if (draw-active-p item)
                               (-
                                (if (and (dialog-item-enabled-p item)
                                         (default-button-p item))
                                  #x777777
                                  *tool-back-color*)
                                (if (pushed-state item)
                                  #x333333
                                  0))
                               *light-gray-color*)
                              back-color)
            (when (keywordp back-color)
              (#_SetThemeBackground (cdr (assoc back-color *theme-brushes-alist*))
               (view-pixel-depth item) (view-color-p item)))
            (#_eraserect rect))))
      (when (frame-p item)
        (let ((frame-color (frame-color item))) 
        (with-fore-color (if (numberp frame-color) frame-color *black-color*)
          (when (keywordp frame-color)
            (#_setThemePen (cdr (assoc frame-color *theme-brushes-alist*)) 
             (view-pixel-depth item) (view-color-p item)))  
          (rlet ((rect :rect :topleft #@(0 0) 
                       :botright (view-size item)))
            (#_framerect rect))))))))

(defmethod text-color ((b 3d-button))
  (part-color b :text))

#+carbon-compat
(defmethod text-position ((b 3d-button))
  (let* ((size (view-size b))
         (height (point-v size))
         (width (point-h size))
         (text (dialog-item-text b)))
    (multiple-value-bind (ff ms)(view-font-codes b)    
      (multiple-value-bind (a d w l)(font-codes-info ff ms)
        (declare (ignore  a w))
        (multiple-value-bind (string-width nlines)
                             (font-codes-string-width-with-eol text ff ms)
        (make-point (max (ash (- width string-width) -1) 2)
                    (- height (+ (* (1- nlines) (font-codes-line-height ff ms))(+ 3 d l)))))))))

(defmethod view-draw-text ((item 3d-button) offset)
  (when (and (view-window item) (dialog-item-text item) (string-not-equal (dialog-item-text item)
                                                                          ""))
    ;; text-position is the bottom of the text - the first line thereof
    (let* ((text-pos (add-points offset (text-position item)))
           (max-width (- (point-h (view-size item)) 5)))
      (with-fore-color #+carbon-compat (or (text-color item) *red-color*)
                       #-carbon-compat
                       (if (and (or (not (dialog-item-enabled-p item))
                                    (not (draw-active-p item))) 
                                (color-or-gray-p item))
                         *gray-color*
                         (or (text-color item)
                              *black-color*))
        (with-back-color *red-color* ; ## likely not needed...
          ;(if (and (dialog-item-enabled-p item)
          ;         (default-button-p item))
          ;  (or (button-default-color item) (darker-color item))
          ;  (or (button-color item) (get-back-color (view-window item))))
          (let* ((curstr (dialog-item-text item))
                 (eol (position #\newline curstr)))
            (if (not eol)
              (progn
                (unless (text-color item)
                                        (#_SetThemeTextColor 
                     ; (if (draw-active-p item) #$kThemeTextColorDialogActive #$kThemeTextColorDialogInactive)
                                          (if (draw-active-p item)
                                              (if (pushed-state item)
                                                  #$kThemeTextColorPushButtonPressed
                                                  #$kThemeTextColorPushButtonActive)
                       #$kThemeTextColorPushButtonInactive)
                     (view-pixel-depth item) (view-color-p item)))
                #+carbon-compat
                (rlet ((rect :rect :topleft offset :bottomright (view-size item)))
                  (incf (pref rect :rect.top)
                        (ceiling
                         (- (point-v (view-size item))
                            (font-line-height)
                            1)
                         2))
                  (draw-theme-text-box curstr rect :center :end))
                #-carbon-compat
                (progn
                  (#_moveto :long text-pos)
                  (with-pstrs ((button-string curstr))
                    (#_TruncString max-width button-string #$truncEnd)
                    (#_drawstring button-string))))
              (let* ((v-delta (multiple-value-call 'font-codes-line-height (view-font-codes item)))
                     (vpos (point-v text-pos))
                      (pos 0))
                  (loop
                    (#_moveto :word (point-h text-pos) :word vpos)
                    (with-pstrs ((button-string curstr pos eol))
                      (#_TruncString max-width button-string #$truncEnd) 
                      (#_drawstring button-string))
                    (when (null eol)(return))
                    (setq vpos (+ vpos v-delta))
                    (setq pos  (1+ eol))
                    (setq eol (position #\newline curstr :start pos))
                    )))))))))

) ; end redefine

; #+carbon-compat
(defmethod view-activate-event-handler :before ((item 3d-button))
  (invalidate-view item))

; #+carbon-compat
(defmethod view-deactivate-event-handler :before ((item 3d-button))
  (invalidate-view item))

#|
(make-instance 'window
  :theme-background T
  :view-subviews
  (list
   (make-instance '3d-button :view-position #@(5 5) :view-size #@(40 20) :dialog-item-text "hello" 
                  :view-font :small-emphasized-system-font)
   (make-instance '3d-button :view-position #@(50 5) :view-size #@(40 20) :dialog-item-text "hello"
                  :dialog-item-enabled-p T
                  :view-font :system-font)))


(make-instance 'window
  :back-color ccl::*lighter-gray-color*
  :view-subviews
    (list
      (make-instance '3d-button
        :view-size #@(30 40)
        :dialog-item-text "testing
RRR")))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; POOF BUTTON

(let ((*warn-if-redefine* nil)
      (*warn-if-redefine-kernel* nil))

(defmethod view-draw-contents ((p poof-button))
  ;; ## These are almost all the same as for bar-dragger...  
  (with-focused-view p
    (rlet ((r :rect
              :topleft 0
              :bottomright (view-size p)))
      (cond
       ((appearance-available-p)
        (#_DrawThemePlacard r (appearance-theme-state p)))
       #-carbon-compat
       (T
        (#_FrameRect r)
        (#_insetrect :ptr r :long #@(1 1))      
        (#_eraserect r)))
    (cond
     #+(and carbon-compat (not alice))
     ((osx-p)
      (#_OffsetRect r -1 -1)
      (#_InsetRect r 4 4)
      (decf (pref r :rect.right))
      (draw-dragger r :vertical (draw-active-p p)))
     (T
      (with-fore-color (if (draw-active-p p) *black-color* *gray-color*)
        #+carbon-compat
        (#_SetThemeTextColor (if (draw-active-p p) #$kThemeTextColorPlacardActive #$kThemeTextColorPlacardInactive)
         (view-pixel-depth p) (view-color-p p))
        (draw-vertical-dragger)))))))

#+carbon-compat
(defmethod initialize-instance ((view new-mini-buffer) &key poof-button &aux (size (if (osx-p) #@(15 16) #@(16 16))))
  (call-next-method)
  (when poof-button
    (make-instance 'poof-button
      :view-size size
      :view-position (subtract-points (view-size view) size)
      :view-nick-name 'poof
      :view-container view)))

) ; end redefine

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BAR DRAGGER

(defun draw-horizontal-dragger ()
  (#_moveto :word 6 :word 4)
  (#_line :long #@(0 6))
  (#_line :long #@(-3 -3))
  (#_line :long #@(2 -2))
  (#_line :long #@(0 3))
  (#_line :long #@(-1 -1))
  (#_moveto :word 9 :word 4)
  (#_line :long #@(0 6))
  (#_line :long #@(3 -3))
  (#_line :long #@(-2 -2))
  (#_line :long #@(0 3))
  (#_line :long #@(1 -1)))

(let ((*warn-if-redefine* nil)
      (*warn-if-redefine-kernel* nil))

(defun draw-vertical-dragger ()
  (#_moveto :word (if (osx-p) 4 5) :word 6)
  (#_line :long #@(6 0))
  (#_line :long #@(-3 -3))
  (#_line :long #@(-2 2))
  (#_line :long #@(3 0))
  (#_line :long #@(-1 -1))
  (#_moveto :word (if (osx-p) 4 5) :word 9)
  (#_line :long #@(6 0))
  (#_line :long #@(-3 3))
  (#_line :long #@(-2 -2))
  (#_line :long #@(3 0))
  (#_line :long #@(-1 1)))

(defmethod view-draw-contents ((view bar-dragger))
  (with-focused-view view
    (rlet ((rect :rect
                   :topleft (if (appearance-available-p) #@(0 0) #@(1 1))
                   :bottomright (if (not (osx-p)) 
                                  (+ (view-size view) #@(1 1))
                                  (view-size view))))
      (if (appearance-available-p)
        (#_DrawThemePlacard rect (appearance-theme-state view))
        #-carbon-compat
        (#_eraserect rect))
      (#_InsetRect rect 4 4)
      (#_OffsetRect rect -1 -1)
      (if #-alice (osx-p) #+alice nil
        (draw-dragger rect (dragger-direction view) (draw-active-p view))                 
        (with-fore-color (if (draw-active-p view) *black-color* *gray-color*)
          #+carbon-compat
          (#_SetThemeTextColor (if (draw-active-p view) #$kThemeTextColorPlacardActive #$kThemeTextColorPlacardInactive)
           (view-pixel-depth view) (view-color-p view))
          (case (dragger-direction view)
            (:horizontal
             (draw-horizontal-dragger))
            (:vertical
             (draw-vertical-dragger))))))))

(defmethod view-cursor ((view bar-dragger) where)
  (declare (ignore where))
  (if (eq (dragger-direction view) :vertical)
    *vertical-ps-cursor*
    #+carbon-compat
    :resize-left-right-cursor
    #-carbon-compat 
    *horizontal-ps-cursor*))

) ; end redefine

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ELLIPSIZED TEXT DIALOG ITEM

(let ((*warn-if-redefine* nil)
      (*warn-if-redefine-kernel* nil))

#+carbon-compat ; make sure this is not patched beyond MCL 5 if Digitool adds the fix!
(defun draw-theme-text-box (text rect &optional (text-justification #$teFlushDefault) truncwhere)
  ;; could add a truncate option and use TruncateThemeText
  (when (keywordp text-justification)
    (setq text-justification
          (case text-justification
            (:center #$tejustcenter)
            (:left #$tejustleft)
            (:right #$tejustright)
            (t #$teFlushDefault))))
  (with-cfstrs ((cftext text))
    (when truncwhere ;; not tested
      (setq truncwhere
          (case truncwhere
            (:end #$truncend)
            (:middle #$truncmiddle)
            (t #$truncend)))
      ;; The CFStringCreateWithCString in with-cfstrs creates an immutable string. It has to be made mutable to be truncated:
      (let ((old cftext)) ; hack...
        (setq cftext (#_CFStringCreateMutableCopy (%null-ptr) 0 cftext))
        (#_CFRelease old))
      (rlet ((foo :boolean))
        (#_TruncateThemeText cftext #$kThemeCurrentPortFont #$Kthemestateactive 
         (- (pref rect :rect.right)(pref rect :rect.left))
         truncwhere
         foo)))       
    (#_Drawthemetextbox cftext #$kThemeCurrentPortFont #$Kthemestateactive t rect text-justification *null-ptr*)))

#+(and ccl-5.0 (not ccl-5.1)) ; only relevant for OSX
(defmethod view-draw-contents ((item ellipsized-text-dialog-item))
  (when (installed-item-p item)
    (with-focused-dialog-item (item)
      (let ((position (view-position item))
            (size (view-size item))
            (handle (dialog-item-handle item)))
        (let ((color-list (slot-value item 'color-list))
              (text-justification (slot-value item 'text-justification))
              (enabled-p (dialog-item-enabled-p item)))
          (rlet ((rect :rect)
                 #+ignore
                 (ps :penstate))
            (rset rect rect.topleft position)
            (rset rect rect.bottomright (add-points position size))          
            ;(setq text-justification
            ;      (or (cdr (assq text-justification
            ;                     '((:left . #.#$tejustleft)
            ;                       (:center . #.#$tejustcenter)
            ;                       (:right . #.#$tejustright))))
            ;          (require-type text-justification 'fixnum)))
            (with-pointers ((tp handle))
              (with-fore-color (getf color-list :text nil)
                (unless (getf color-list :text nil)
                                    (#_SetThemeTextColor 
                   (if (draw-active-p item) #$kThemeTextColorDialogActive #$kThemeTextColorDialogInactive)
                   (view-pixel-depth item) (view-color-p item)))
                ;; use subtle gray cause can't figure out how to draw with theme-background in this case 
                ;; - maybe they won't notice [I did. Terje ;-]
                (with-back-color (or (getf color-list :body nil)
                                     #+ignore (if (and (osx-p)(view-get (view-window item) 'theme-background)) *power-book-back-color*))
                  (#_TextMode (if (or enabled-p (not (getf color-list :text nil))) #$srcCopy #$grayishTextOr)) ;#$srcCopy)
                  (#_EraseRect :ptr rect)
                  (when (compress-text-p item)
                    (#_TextFace (ash 1 #$condense)))
                  (draw-theme-text-box (dialog-item-text item) rect (or text-justification :left) t)
                  ;(draw-ellipsized-text-in-rect tp (#_GetHandleSize handle) rect
                  ;                              text-justification (compress-text-p item))
                  )))
            #+ignore
            (unless enabled-p
              (#_GetPenState ps)
              (#_PenPat *gray-pattern*)
              (#_PenMode 11)
              (#_PaintRect rect)
              (#_SetPenState ps))))))))

#+ccl-5.1
(defmethod view-draw-contents ((item ellipsized-text-dialog-item))
   (when (installed-item-p item)
     (with-focused-dialog-item (item)
       (let (;(position (view-position item))
             ;(size (view-size item))
             ;(handle (dialog-item-handle item))
             )
         (let ((color-list (slot-value item 'color-list))
               (text-justification (slot-value item 'text-justification))
               (enabled-p (dialog-item-enabled-p item)))
           (with-item-rect (rect item)
            ;rlet ((rect :rect)
                  ;(ps :penstate)
                  ;)
             ;(rset rect rect.topleft position)
             ;(rset rect rect.bottomright (add-points position size))
             ;(setq text-justification
             ;      (or (cdr (assq text-justification
             ;                     '((:left . #.#$tejustleft)
             ;                       (:center . #.#$tejustcenter)
             ;                       (:right . #.#$tejustright))))
             ;          (require-type text-justification 'fixnum)))
             (progn ;with-pointers ((tp handle))
               (with-fore-color (or (getf color-list :text nil) #-ccl-5.1 *red-color*)
                 ;; use subtle gray cause can't figure out how to draw with theme-background in this case - maybe they won't notice
                 (with-back-color (or (getf color-list :body nil)
                                      #+ignore (if (and (osx-p)(view-get (view-window item) 'theme-background)) *power-book-back-color*))
                   (if (not enabled-p)
                     (#_TextMode #$grayishTextOr))
                   (if (compress-text-p item)
                     (#_TextFace (ash 1 #$condense)))
                   (unless (getf color-list :text nil)
                                          (#_SetThemeTextColor 
                      (if (draw-active-p item) #$kThemeTextColorDialogActive #$kThemeTextColorDialogInactive)
                      (view-pixel-depth item) (view-color-p item)))
                   (#_EraseRect :ptr rect)
                   ;; ## should also use compress-text-p! done above -but does truncatethemetext know that? should
                   (draw-theme-text-box (dialog-item-text item) rect 
                                        (or text-justification :left) t)
                   ;(draw-ellipsized-text-in-rect tp (#_GetHandleSize handle) rect
                   ;                             text-justification (compress-text-p item))
                   )))
             #+ignore
             (unless enabled-p
               (#_GetPenState ps)
               (#_PenPat *gray-pattern*)
               (#_PenMode 11)
               (#_PaintRect rect)
               (#_SetPenState ps))))))))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; POP-UP MENU

;; Compare with code in platinum-pop-up-menu (which should be eliminated?)
;; Also check those for button-dialog-item - can be simplified like below!

#+ccl-5.0
(setq *use-pop-up-control* t) ;; Use modern pop-up also forOS9

#-carbon-compat
(setq *draw-inactive-dialog-items-as-disabled* t)

(let ((*warn-if-redefine* nil)
      (*warn-if-redefine-kernel* nil))

#+carbon-compat
(defmethod view-deactivate-event-handler :before ((menu pop-up-menu))
  (when (and #-carbon-compat (appearance-available-p)
             (installed-item-p menu)
             (control-handle menu)) ; control-handle may still be NIL even if installed-item-p
    (with-focused-dialog-item (menu)
      (#_DeactivateControl (control-handle menu)))))

#+carbon-compat
(defmethod view-activate-event-handler :before ((menu pop-up-menu))
  (when (and #-carbon-compat (appearance-available-p)
             (installed-item-p menu)
             (menu-enabled-p menu)
             (control-handle menu)) ; control-handle may still be NIL even if installed-item-p
    (with-focused-dialog-item (menu)
      (#_ActivateControl (control-handle menu)))))

) ; end redefine



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PULL DOWN MENU

(in-package :ccl)

;; I recommend that the crescent is eliminated from the pull-down menu implementation. 
;; The crescent should be drawn by the command pane, if at all. 
;; Better to draw a triangle next to the pull-down menu to visualize that it is a menu (there is a call to do this in carbon).
;; Better yet to spread the items in the pull down menu in the command pane as done in e.g. Apple's Mail program.
;; I also recommend separating the code for drawing the pull-down menu from the code for the pop-up-menu, as they now diverge.

(let ((*warn-if-redefine* nil))

(defclass pull-down-menu (pop-up-menu)
  ((:crescent :initarg :crescent :initform nil :reader crescent)
   (arrow :initarg :arrow :initform NIL))
  (:default-initargs
    :auto-update-default nil
    :default-item 0))

) ; end redefine

;; view-draw-contents on pull-down-menu can benefit from a good cleaning to eliminate unecessarry code...

; borrowed from method on pop-up-menu in MCL 5:
(defmethod view-draw-contents ((menu pull-down-menu))
  (let* ((text (menu-title menu))
         (ti-rect (pop-up-menu-title-rect menu))
         (no-title (or (null  text)(equal text "")))
         (enabled (menu-enabled-p menu))
         (colorp (and (color-or-gray-p menu)(window-color-p (view-window menu))))
         (disabled-color (if (and (not enabled) colorp)
                           *gray-color*))
         (title-color (or disabled-color
                          (part-color menu :menu-title))))
    (with-focused-dialog-item (menu)  ; take font from item, draw in containers coords - this is the other thing that dialog item gives us
      (multiple-value-bind (a d w leading)(view-font-codes-info menu)
        (declare (ignore a w))
        (rlet ((a-rect :rect))
          (copy-record (pop-up-menu-rect menu) :rect a-rect)
          (let ((mi-title (get-menu-body-text menu)))
            (with-fore-color disabled-color
              (unless no-title
                (with-fore-color title-color ; 21-Jun-91 -wkf                
                  (with-back-color (part-color menu :title-background)
                    (#_EraseRect :ptr ti-rect)
                    (#_MoveTo :word (+ (rref ti-rect rect.left) 3) ; (+ (point-h pos) 3)
                     :word (- (rref ti-rect rect.bottom) (+ d leading)))
                    (if (and (osx-p) #+ccl-5.0 (theme-background-p menu) t)
                      #+carbon-compat
                      (draw-theme-text-box text ti-rect)
                      #-carbon-compat
                      NIL
                      (with-pstrs ((di-title text))
                        (#_DrawString :ptr di-title))))))
              ;  (#_OffsetRect :ptr a-rect :long #@(0 -1))
              (with-back-color (part-color menu :menu-body) ; 10-Nov-92 -straz
                ;(#_FillRect :ptr a-rect :ptr *white-pattern*)
                ;(when (not (control-handle menu))
                ;  (#_EraseRect a-rect)
                ;  
                ;  )  ;; same as above
                (#_InsetRect :ptr a-rect :long #@(1 1))
                (with-fore-color (or title-color
                                     #+carbon-compat
                                     *red-color*
                                     #-carbon-compat
                                     (if (and (appearance-available-p) (not (draw-active-p menu)))
                                                   *gray-color*
                                                   *black-color*))
                  #+carbon-compat
                  (unless title-color
                    (#_SetThemeTextColor
                           (if (draw-active-p menu) #$kThemeTextColorDialogActive #$kThemeTextColorDialogInactive)
                           (view-pixel-depth menu) (view-color-p menu)))
                  (let* ((left (+ (rref a-rect rect.left) 6))
                         (right (rref a-rect rect.right))
                         (bottom (rref a-rect rect.bottom)))
                    (#_MoveTo :word left :word  (- bottom (+ leading 1 d)))
                    (with-clip-rect-intersect a-rect
                      (cond
                       #+carbon-compat
                       (t ;(and (osx-p) (theme-background-p menu))
                        (incf (rref a-rect :rect.left) 3)
                        (unless (osx-p)
                          (#_offsetRect a-rect 0 1))
                        (draw-theme-text-box mi-title a-rect))  ;; will it crop?
                       #-carbon-compat
                       (T
                        (draw-string-crop mi-title (- right left 0 12))))
                      (#_MoveTo :word (- right (+ 4 11))
                       :word (- (ash (+ bottom (rref a-rect :rect.top)) -1)
                                2)))))
                #+carbon-compat
                (when (slot-value menu 'arrow)
                  (rlet ((bounds :rect 
                                 :top (+ (rref a-rect rect.top) 6)
                                 :left (- (rref a-rect rect.right) 7)
                                 :bottom (rref a-rect rect.bottom)
                                 :right (rref a-rect rect.right)))
                    (#_DrawThemePopupArrow bounds #$kThemeArrowDown #$kThemeArrow5pt 
                     (appearance-theme-state menu) (%null-ptr) 0)))))))
        (unless (or enabled colorp)
          (paint-menu-gray menu))))))

(let ((*warn-if-redefine* nil)
      (*warn-if-redefine-kernel* nil))

#+carbon-compat ;; ripped from MCL 5
(defmethod view-click-event-handler ((menu pull-down-menu) where)
  (declare (ignore where))
  (when (menu-enabled-p menu)
    (multiple-value-bind (a d w leading) (view-font-codes-info menu)
      (declare (ignore a w))
      (with-focused-dialog-item (menu)  ; << focus she said
        (let ((orig-back (or (part-color menu :menu-body) *white-color*))
              (orig-fore  (or (part-color menu :menu-title) *black-color*)))
          (with-back-color orig-fore
            (with-fore-color orig-back
              #+carbon-compat
              (#_SetThemeTextColor #$kThemeTextColorRootMenuSelected 
               (view-pixel-depth menu) (view-color-p menu))
              (let* ((rect (pop-up-menu-rect menu))
                     (mi-title (get-menu-body-text menu))
                     (left (+ (rref rect rect.left) 7))  ;(if pull-down-p 6 (max 6 w))))
                     #-carbon-compat
                     (right (rref rect rect.right))
                     (bottom (rref rect rect.bottom)))
                ; (#_Eraserect rect)
                #+carbon-compat  
              (#_DrawThemeMenuTitle rect rect #$kThemeMenuSelected 0 (%null-ptr) 0)           
                (#_moveto :word left :word  (- bottom (+ leading 2 d)))              
                (with-clip-rect-intersect rect
                  (cond
                   #+carbon-compat 
                   (t ;(and (osx-p) #+ccl-5.0 (theme-background-p menu) t) 
                    (rlet ((a-rect :rect))
                      (copy-record (pop-up-menu-rect menu) :rect a-rect)
                      (#_InsetRect :ptr a-rect :long #@(1 1))
                      (incf (rref a-rect :rect.left) 3) 
                      (unless (osx-p)
                        (#_offsetRect a-rect 0 1))                  
                      (draw-theme-text-box mi-title a-rect)))
                   #-carbon-compat
                   (T
                    (draw-string-crop mi-title (- right left))))))))) ; (if pull-down-p 0 12)))))))
        (menu-select menu 0)))))

) ; end redefine

; new:

(defmethod view-default-size ((menu pull-down-menu))
  ;; allow space for the triangle:
  #+carbon-compat
  (with-font-focused-view menu
    (rlet ((size :point)
           (baseline :signed-word))
      (with-cfstrs ((cftext (ccl::get-menu-body-text menu))) 
        (#_GetThemeTextDimensions cftext 
         #$kThemeCurrentPortFont 
         (ccl::appearance-theme-state menu)
         NIL
         size
         baseline))
      (make-point (+ (point-h (%get-point size)) 15) 
                  (point-v (call-next-method)))))
  #-carbon-compat
  (add-points #@(10 0) (call-next-method)))

#-carbon-compat ; new
(defmethod view-deactivate-event-handler :after ((view pull-down-menu))
  (invalidate-view view))

#-carbon-compat ; new
(defmethod view-activate-event-handler :after ((view pull-down-menu))
  (invalidate-view view))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INSPECTOR, PROCESSES, BACKTRACE WINDOWS

;; For consistency with Aqua, I recommend that the pull-down menu of the command pane is substituted by placing 
;; the command items in the pane bar, with an optional >> at the end for extra items, similar to what is found in Apple's mail program.

(in-package :inspector)

(defmethod view-draw-contents :around ((view command-pane-mixin))
  ;(if (not (ccl::theme-background-p view))
   ; (call-next-method)
    (progn ; with-back-color (when (appearance-available-p) *red-color*)
      (when (appearance-available-p) 
        (#_SetThemeBackground #$kThemeBackgroundWindowHeader 
         (ccl::view-pixel-depth view) (ccl::view-color-p view))) ; available with appearance 1.0
      (with-focused-dialog-item (view)
        (ccl::with-item-rect (rect view)
          #-carbon-compat
          (when (ccl::draw-active-p view)
            (#_DrawThemeMenuBarBackground rect #$kThemeMenuBarNormal 0))
          (unless (#+carbon-compat osx-p)
            (decf (rref rect rect.top))
            (decf (rref rect rect.left)))
          #-carbon-compat
          (unless (ccl::draw-active-p view)
            (#_DrawThemeWindowHeader rect (ccl::appearance-theme-state view)))
          #+carbon-compat
          (#_DrawThemeWindowHeader rect (ccl::appearance-theme-state view))
          #+carbon-compat 
          (when (osx-p)
            (setf (rref rect rect.top) (- (rref rect rect.bottom) 2))
            (#_DrawThemeSeparator rect (ccl::appearance-theme-state view))
          )))
      (call-next-method)
))  ;)

(defmethod view-deactivate-event-handler :after ((view command-pane-mixin))
  (invalidate-view view #-carbon-compat t #+carbon-compat (osx-p)))

(defmethod view-activate-event-handler :after ((view command-pane-mixin))
  (invalidate-view view #-carbon-compat t #+carbon-compat (osx-p)))

(let ((*warn-if-redefine* nil))

#+carbon-compat
(defclass command-pane (command-pane-mixin #| bottom-line-mixin |# view)
  ())

) ; end redefine

(let ((*warn-if-redefine* nil)
      (*warn-if-redefine-kernel* nil))

#+carbon-compat
(defclass inspector-window (undo-view-mixin window)
  ((selected-pane :initform nil :accessor selected-pane)
   (user-title :accessor user-title))
  (:default-initargs 
    :theme-background t
    :window-title nil))

#+carbon-compat ; same functionality, but uses with-highlite-mode.
(defun %invert-region (rgn)
  ; (#_lmsethilitemode (logand (lognot (ash 1 #$hiliteBit)) (#_lmgethilitemode)))
  (ccl::with-hilite-mode 
    (#_InvertRgn :ptr rgn)))

;; ## better to instead move contents of hilight-selection to view-draw-contents, then make hilight-selection invalidate view?
(defun set-selection (view new-selection)
  (setf (selection view) new-selection)
  (highlight-selection view)
  (unless (#+carbon-compat osx-p) ; redraws the selection, as just inverting it doesn't work, but should only invalidate selected area...
    (invalidate-view view)))

(defclass inspector::inspector-editor (window)
  ((inspector::done-fun :initarg :done-fun :accessor inspector::done-fun)
   (inspector::value :initarg :value :accessor inspector::value)
   (inspector::modcnt :initform -1 :accessor inspector::modcnt))
  (:default-initargs
   #+carbon-compat :theme-background #+carbon-compat t
   #-carbon-compat :back-color #-carbon-compat *tool-back-color* 
   :WINDOW-TYPE :DOCUMENT
   :view-position ':CENTERED
   :view-size #@(452 175)
   :CLOSE-BOX-P t
   :view-FONT (ccl::sys-font-spec))) ;'("Chicago" 12 :SRCOR :PLAIN)))

#+carbon-compat
(defmethod set-view-size ((pane command-pane-mixin) h &optional v)
  (let* ((size (make-point h v))
         (h (point-h size))         
         (menu (view-named 'command-menu pane))
         ;(button (elt (view-subviews pane) 0))
         ;(mv (if menu (point-v (view-size menu)) 0))
         ;(v (max mv (point-v size) (+ 6 (point-v (view-size button)))))
         )    
    (call-next-method pane h *command-pane-min-height*)
    (adjust-subview-positions pane)
    ; or maybe 0 0
    ; the menu is 2 pixels smaller than the button - phooey
    ; makes the crescent look funny (2 pix white above)
    (when menu 
      (set-view-size menu (point-h (view-size menu))(1- (point-v (view-size pane))))
      (set-view-position menu 0 0)) ; (+ 1 (- v mv))))
    size))

#+carbon-compat
(defmethod add-command-pane-items ((command-pane command-pane) &optional (edit-value-button t))
  (let* ((font :small-system-font) ; '("Geneva" 10 :bold))
         ;(width (+ 8 19 (string-width "Commands" font)))        ; 19 is for the little triangle
         ;(height (+ 5 (font-line-height font))) 
         ;(menu-size (make-point width height))
         (resample-button (make-and-size-dialog-item
                           'ccl::3d-button ;'subtle-button
                           :view-nick-name 'resample-button
                           :frame-p t
                           ;:border-p nil
                           :default-button t
                           :dialog-item-text "Resample"
                           :view-font font
                           :dialog-item-action
                           #'(lambda (item)
                               (resample (view-window item)))))
         ;(button-size (view-size resample-button))
         )
    ;(set-view-size resample-button button-size)    
    (add-subviews command-pane
                  (make-instance 'pull-down-menu
                                 :item-display "Commands"
                                 :view-nick-name 'command-menu
                                 :crescent nil ; t
                                 :arrow T
                                 :view-font :small-system-font ; :small-emphasized-system-font ; font ("Geneva" 10 :bold)
                                 :view-size nil
                                 :menu-items nil
                                 :enabledp nil
                                 :auto-update-default nil
                                 :update-function #'(lambda (menu) 
                                                      (install-commands 
                                                       (view-container menu))))
                  resample-button)
    (when edit-value-button
      (add-subviews command-pane                  
                 (make-instance
                   'ccl::3d-button ;button-dialog-item
                   ;:view-size button-size
                   :view-nick-name 'Edit-button
                   :dialog-item-text "Edit Value"
                   :view-font font
                   ;:border-p nil
                   :frame-p t
                   :dialog-item-action
                   #'(lambda (item)
                       (edit-selection (view-window item))))
                   #|
                  (make-instance
                   'button-dialog-item ;'subtle-button
                    :default-button t
                   :view-size button-size
                   :view-nick-name 'inspect-button
                   :dialog-item-text "Inspect"
                   :view-font font
                   ;:border-p nil
                   :dialog-item-action
                   #'(lambda (item)
                       (inspect-selection (view-window item))))|#
                  ))))

; new:
(defmethod view-draw-contents :around ((pane inspector-pane))
  (if (appearance-available-p)
    (with-back-color *red-color*
      (#_SetThemeBackground #$kThemeBrushListViewBackground 
       (ccl::view-pixel-depth pane) (ccl::view-color-p pane))
      ;; ### Erases visible background on top of view, but perhaps this would be better elsewhere??
      (with-focused-view pane
        (rlet ((rect :rect :topleft #@(0 0) :bottom 3 :right (point-h (view-size pane))))
          (#_eraseRect rect)))
      (call-next-method)
      #+ignore
      (when (and (osx-p)
                 ; ugly... perhaps timely to clean up the class hierarchy...?
                 #+ignore
                 (not (and (typep pane 'backtrace-inspector-pane)
                           (eq (view-nick-name pane) 'stack-pane)))) 
        (with-focused-dialog-item (pane)
          (ccl::with-item-rect (rect pane)
            (#_DrawThemeListBoxFrame rect (ccl::appearance-theme-state pane))))))
    (call-next-method)))

#+carbon-compat
(defun draw-inspector-view-internal (view &optional
                                         (start-line (start-line view)) end-line (vpos 0))
  (let ((inspector (inspector view)))
    (when inspector
      (with-errorfree-printing
        (with-focused-view view             ; simple-view's don't get focused
          (with-back-color *red-color*
            (#_SetThemeBackground #$kThemeBrushListViewBackground 
             (ccl::view-pixel-depth view) (ccl::view-color-p view))
            (let* ((pretty-p  (pretty-p view))
                   (*print-pretty* pretty-p)
                   (*print-circle* (and pretty-p *print-circle*))
                   (*print-right-margin* 
                    (floor (point-h (view-size view)) (string-width "N")))
                   (cache-p (and (not pretty-p) (cache-p view)))
                   (real-end-line (or end-line (inspector-line-count inspector))))
              (with-preserved-stream-font view
                (set-stream-font view '(:srccopy))
                (catch (page-truncation-tag view)
                  (if (eql 0 vpos)
                    (top-of-page view)
                    (progn
                      (setf (newline-pending? view) nil)
                      (#_MoveTo :word (margin view) :word vpos)))
                  (if cache-p
                    (draw-cached view start-line real-end-line)
                    (draw-uncached view start-line real-end-line))
                  (unless end-line
                    (clear-to-eop view)))))))))))

) ; end warn if redefine

;;;; BACKTRACE:

(let ((*warn-if-redefine* nil))

(defclass backtrace-command-pane (command-pane-mixin #| bottom-line-mixin |# view)
  ())

) ; end redefine

(defmethod view-draw-contents :before ((view backtrace-info-pane))
  (with-focused-dialog-item (view)
  (ccl::with-item-rect (rect view)
    (unless (#+carbon-compat osx-p)
      (decf (rref rect rect.left))
      (incf (rref rect rect.bottom)))      
    (#_DrawThemePlacard rect (ccl::appearance-theme-state view)))))

; new
(defmethod view-deactivate-event-handler :after ((view backtrace-info-pane))
  (invalidate-view view #+carbon-compat (osx-p)))

; new
(defmethod view-activate-event-handler :after ((view backtrace-info-pane))
  (invalidate-view view #+carbon-compat (osx-p)))

(let ((*warn-if-redefine* nil)
      (*warn-if-redefine-kernel* nil))

#+carbon-compat ; from MCL 5
(defclass backtrace-window (undo-view-mixin backtrace-view window) ()
  (:default-initargs
    :theme-background T
    :view-size (car *backtrace-sizes*)
    :view-position (cadr *backtrace-sizes*)
    :top-pane-size (caddr *backtrace-sizes*)
    :window-title "Backtrace"))

; The changed constant below is likely the width of a scroller... and should extract this value for more flexibility!
; from MCL 5
(defmethod adjust-subview-positions ((view backtrace-view))
  (let* ((size (view-size view))
         (width (point-h size))
         (height (point-v size))
         (command-pane (view-named 'command-pane view))   ; height unchanged top
         (stack-pane (view-named 'stack-pane view))  ; height unchanged second
         (info-pane (view-named 'info-pane view))  ; height unchanged bottom         
         (stack-frame-pane (view-named 'stack-frame-pane view))
         info-pane-height h v)
    (when (and stack-pane info-pane stack-frame-pane command-pane)   ; NIL on initialize-instance of backtrace-window
      (setq h (point-h (view-position command-pane)))
      (adjust-subview-positions info-pane)
      (setq info-pane-height (point-v (view-size info-pane)))
      (setq v (+ (point-v (view-size command-pane))
                 (point-v (view-size stack-pane))))
      (set-view-size command-pane width (point-v (view-size command-pane)))
      ;(adjust-subview-positions command-pane)
      (set-view-position stack-pane h (point-v (view-size command-pane)))
      (set-view-size stack-pane width (point-v (view-size stack-pane)))
      (set-view-position stack-frame-pane h  v)
      (set-view-size stack-frame-pane width (- height v info-pane-height -3))
      (set-view-position info-pane h (- height info-pane-height))
      (set-view-size info-pane (- width 14)(point-v (view-size info-pane)) ))))

(defmethod adjust-subview-positions ((view backtrace-info-pane))
  (let* ((h 6) 
         (labels (label-dialog-items view))
         (value-width (+ 6 (string-width "#xFFFFFFFF" (view-font (car labels))))))
    (do ((labs labels (cdr labs))
         (vals (value-dialog-items view) (cdr vals)))
        ((null labs))
      (let ((lab (car labs))
            (val (car vals)))
        (let ((size (view-default-size lab)))
          (set-view-size lab (subtract-points size (if (#+carbon-compat osx-p) #@(0 0) #@(0 2))))
          (set-view-position lab h (if (#+carbon-compat osx-p) 1 2))
          (setq h (+ h (point-h size) ))
          (set-view-position val h (if (#+carbon-compat osx-p) 1 2))
          (set-view-size val value-width (- (point-v size) (if (#+carbon-compat osx-p) 0 2)))
          (setq h (+ h value-width )))))
      (set-view-size view  ;; seems to be redundant as superceded by adjust-subview-positions on backtrace-view...
                     (- (point-h (view-size (view-container view))) 17)
                     (point-v (view-size view)))))

(defmethod initialize-instance ((view backtrace-view) &rest initargs &key 
                                view-container info (top-pane-size #@(100 82)))
  (declare (dynamic-extent initargs))
  (apply #'call-next-method view :view-container nil initargs)
  (let* ((command-pane (make-instance 'backtrace-command-pane
                         :view-nick-name 'command-pane
                         ;:view-size #@(100 17)
                         :view-container view))
         (stack-inspector (make-instance 'stack-inspector :info info))
         (error-frame (inspector-object stack-inspector))
         (stack-frame-pane (make-instance 'backtrace-inspector-pane
                                          :inspector (make-instance 'stack-frame-inspector
                                                                    :object error-frame)
                                          :pane-splitter :top
                                          :pane-splitter-cursor *arrow-cursor*
                                          :pane-splitter-class 'dragging-pane-splitter
                                          :grow-box-p nil
                                          :view-nick-name 'stack-frame-pane
                                          :view-container view
                                          :help-spec 14072)))    
    (set-view-size command-pane 100 (point-v (view-size (view-named 'menu command-pane))))
    (make-instance 'backtrace-inspector-pane
      :inspector stack-inspector
      :inspector-view-class 'stack-inspector-view
      :pane-splitter :bottom
      :double-bottom-line t
      :pane-splitter-class 'dragging-pane-splitter
      :pane-splitter-cursor *arrow-cursor*
      :pane-splitter-length 8
      :view-size top-pane-size
      :view-nick-name 'stack-pane
      :view-container view
      :cache-p (< (frame-count error-frame) 5000)
      :help-spec 14071)
    (make-instance 'backtrace-info-pane
                   :stack-frame-inspector (inspector stack-frame-pane)
                   :view-nick-name 'info-pane
                   :view-container view
                   :view-size (make-point (point-h top-pane-size) 15)
                   :help-spec 14070)
    (when view-container
      (set-view-container view view-container))))

#+carbon-compat ; ripped from MCL 5
(defmethod initialize-instance ((view backtrace-info-pane) &key)
  (call-next-method)
  (let* ((bfont (if (osx-p) :label-font :small-system-font)) (font bfont)) ; '("monaco" 9 :bold)
    (setf (label-dialog-items view)
          (list (make-instance 'static-text-dialog-item
                               :dialog-item-text "Size:"
                               :view-font bfont
                               :view-container view
                               :help-spec 14073)
                (make-instance 'static-text-dialog-item
                               :dialog-item-text "PC:"
                               :view-font bfont
                               :view-container view
                               :help-spec 14075)
                (make-instance 'static-text-dialog-item
                               :dialog-item-text "Address:"
                               :view-font bfont
                               :view-container view
                               :help-spec 14074)))
    (setf (value-dialog-items view)
          (list (make-instance 'static-text-dialog-item
                               :dialog-item-text ""
                               :view-nick-name 'frame-size
                               :view-font font
                               :view-container view
                               :help-spec 14073)
                (make-instance 'static-text-dialog-item
                               :dialog-item-text ""
                               :view-nick-name 'program-counter
                               :view-font font
                               :view-container view
                               :help-spec 14075)
                (make-instance 'static-text-dialog-item
                               :dialog-item-text ""
                               :view-nick-name 'frame-address
                               :view-font font
                               :view-container view
                               :help-spec 14074)))))

#+carbon-compat ;; ripped from MCL 5
(defmethod initialize-instance ((view backtrace-command-pane) &key)
  (call-next-method)
  (add-subviews view
                (make-instance
                  'ccl::3d-button ;button-dialog-item
                  ;:view-size button-size
                  :view-nick-name 'Edit-button
                  :dialog-item-text "Edit Value"
                  :view-font :small-system-font ; '("geneva" 10 :bold)
                  ;:view-position #@(100 0)
                  ;:border-p nil
                  :dialog-item-enabled-p nil
                  :frame-p t
                  :dialog-item-action
                  #'(lambda (item)
                      (edit-selection
                       (inspector-view
                        (view-named 'stack-frame-pane (view-window item)))))
                  :help-spec 14084))
  (let ((command-menu (make-instance 'pull-down-menu
                        :update-function 'update-backtrace-command-menu
                        :item-display "Commands"
                        :crescent nil ; t
                        :arrow T
                        :view-size nil
                        :view-position #@(1 1)
                        :auto-update-default nil
                        ;:justify :right
                        :view-nick-name 'menu
                        :view-font :small-system-font  ; :small-emphasized-system-font ; '("Geneva" 10 :bold)
                        :view-container view
                        :help-spec 14076)))
    ;(setf (command-menu (view-container view)) command-menu)
    (add-new-item command-menu
                  "Edit Definition"
                  #'(lambda ()
                      (edit-stack-definition
                       (inspector-view
                        (view-named 'stack-pane (view-window command-menu)))))
                  :help-spec 14081)
    (add-new-item command-menu
                  "Inspect Function"
                  #'(lambda ()
                      (inspect-selection
                       (inspector-view (view-named 'stack-pane (view-window command-menu)))))
                  :help-spec 14082)
    (add-new-item command-menu 
                  "Return from frameÉ"
                  #'(lambda () (backtrace-return-from-frame (view-window command-menu)))
                  :help-spec '(14078 1 2))
    (add-new-item command-menu 
                  "Restart frameÉ"
                  #'(lambda () (backtrace-restart-frame (view-window command-menu)))
                  :help-spec '(14079 1 2))
    (add-new-item command-menu
                  "RestartsÉ"
                   #'(lambda () (backtrace-choose-restart (view-window command-menu)))
                  :help-spec 14080)
    (add-new-item command-menu
                  "-"
                  nil
                  :disabled t)
    (add-new-item command-menu
                  "Edit ValueÉ"
                  #'(lambda ()
                      (edit-selection
                       (inspector-view
                        (view-named 'stack-frame-pane (view-window command-menu)))))
                  :help-spec '(14077 1 2))
    (add-new-item command-menu
                  "Inspect Value"
                  #'(lambda ()
                      (inspect-selection
                       (inspector-view (view-named 'stack-frame-pane (view-window command-menu)))))
                  :help-spec 14083)
    (add-new-item command-menu
                  "-"
                  nil
                  :disabled t)
    (add-new-item command-menu
                  "Inspect break condition"
                  #'(lambda ()
                      (inspect
                       (break-condition
                        (inspector-object
                         (find-named-sibling view 'stack-pane))))))
    (flet ((stack-pane-inspector-view (command-view)
             (inspector-view (find-named-sibling command-view 'stack-pane))))
      (let (item)
        (setq item
              (add-new-item command-menu
                            "Show all frames"
                            #'(lambda ()
                                (switch-show-all-frames (stack-pane-inspector-view view)))
                            :update-function
                            #'(lambda ()
                                (update-show-all-frames item (stack-pane-inspector-view view))))))
      (let (item)
        (setq item
              (add-new-item command-menu
                            "Default show all frames"
                            #'(lambda ()
                                (switch-default-show-all-frames (inspector-view (find-named-sibling view 'stack-pane))))
                            :update-function
                            #'(lambda ()
                                (update-default-show-all-frames item))))))))

; new, but should be merged with main method!
(defmethod view-draw-contents :around ((p dragging-pane-splitter))
  ;; Hack to draw the two pane splitters as if they were one dragger... but better to merge them into one instead!
  ;(if (not (osx-p))
    ;(call-next-method)
    (let ((top? (eq :bottom (pane-splitter-position (scroll-bar p))))) ; TOP pane splitter (is at bottom of the scroller)...
      (with-focused-view p
        (let ((size (view-size p)))
          (rlet ((rect :rect
                       :top (if top? 0 (- (point-v size)))
                       :left 0
                       :bottom (* (point-v size) (if top? 2 1))
                       :right (- (point-h size) 1)))
            (unless (#+carbon-compat osx-p)
              (#_insetRect rect -1 -1)
              (incf (rref rect rect.left)))
            (#_DrawThemePlacard rect (ccl::appearance-theme-state p))
            (#_OffsetRect rect (if (#+carbon-compat osx-p) -1 -2) -1)
            (#_InsetRect rect 4 3)
            (unless (#+carbon-compat osx-p)
              (incf (rref rect rect.left))
              (#_InsetRect rect 0 1))
            (if (#+(and carbon-compat (not alice)) osx-p)
              (ccl::draw-dragger rect :vertical (ccl::draw-active-p p))
              (let ((origin (view-origin p))
                    (offset (if top? 1 (+ (point-v (view-size p)) 1))))
                  (#_setOrigin (point-h origin)(+ (point-v origin) offset))
                  (ccl::with-temp-rgns (clip)
                    (#_getClip clip)
                    (#_OffsetRgn clip 0 (1+ offset))
                    (#_setClip clip))
                  (with-fore-color (if (ccl::draw-active-p p) *black-color* *gray-color*)
                    #+carbon-compat
                    (#_SetThemeTextColor 
                     (if (ccl::draw-active-p p) #$kThemeTextColorPlacardActive #$kThemeTextColorPlacardInactive)
                     (ccl::view-pixel-depth p) (ccl::view-color-p p))
                    (ccl::draw-vertical-dragger))
)))))))

#+carbon-compat
(defmethod view-draw-contents ((view double-bottom-line))
  (with-focused-dialog-item (view)
    (ccl::with-item-rect (rect view)
      (#_DrawThemeSeparator rect (ccl::appearance-theme-state view))
)))

#+carbon-compat
(defmethod view-activate-event-handler :before ((view double-bottom-line))
  (invalidate-view view #+ccl-5.0 (osx-p)))

#+carbon-compat
(defmethod view-deactivate-event-handler :before ((view double-bottom-line))
  (invalidate-view view #+ccl-5.0 (osx-p)))

) ; end redefine

(in-package :ccl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FRED COMMANDS WINDOW (ed-help)

(let ((*warn-if-redefine* nil)
      (*warn-if-redefine-kernel* nil))

(defclass key-cap (3d-button)
  ((:down-p :initarg :down-p :initform nil :accessor key-cap-down-p)
   (:key-name :initarg :key-name :initform nil :accessor key-cap-name)
   (:color-list :initarg :part-color-list :initform `(:body ,*white-color*)
                :reader part-color-list))
  (:default-initargs :frame-p T))

(defmethod view-draw-contents ((view key-cap))
  (call-next-method))

(defmethod pushed-state ((view key-cap))
  (key-cap-down-p view))

(defmethod dialog-item-text ((view key-cap))
  (key-cap-name view))

) ; end redefine

#+ccl-5.0 ;; This is just to change the fonts... so fix that in the ed-help definition instead of copying this code!
(defmethod initialize-instance :before ((w ed-help-window) &rest rest &key view-subviews)
  (declare (ignore rest))
  (labels ((update-font (subviews)
             (dolist (view subviews)
               (typecase view
                 (static-text-dialog-item
                  (set-view-font view :small-system-font)
                  #+ccl-5.1
                  (when (eq 'fred-title (view-nick-name view))
                    (set-view-position view
                      (add-points (view-position view) #@(0 -1)))))
                 (view
                  (update-font (subviews view)))))))
    (update-font view-subviews)))

;; @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; OPTIONAL PATCHES TO THE APPEARANCE MANAGER MODULE IN THE MCL EXAMPLES FOLDER

;; Load this file after the appearance manager modules for the patches to take effect.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLOCK DIALOG ITEM

;; The Clock-dialog-item of the appearance manager no longer has a negative hour right after midnight during daylight savings time.
;; Reported to Digitool October 21 -2003 - included in MCL 5.1b1.

(in-package :ccl)

#-ccl-5.1
(when (module-loaded-p :clock-dialog-items)

(defmethod set-universal-time ((item clock-dialog-item) utime)
  (multiple-value-bind (second minute hour day month year day-of-week dst-p)
                       (decode-universal-time utime)
    (set-control-data item
                      #$kControlClockLongDateTag
                      :LongDateRec
                      :era       1
                      :year      year
                      :month     month
                      :day       day
                      :hour      (if dst-p
                                   (mod (1- hour) 24)
                                   hour)
                      :minute    minute
                      :second    second
                      :dayOfWeek day-of-week))
  utime)

) ; end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PROGRESS BAR

(in-package :ccl)

; Reported to Digitool on oct-21-2003.
; Covered by new progress-bar-dialog-item by Digitool, which includes it with MCL 5.1.

; Makes sure determinate-p is boolean:

(when (module-loaded-p :tab-bar-view)

#-ccl-5.1
(defmethod set-progress-bar-determinate-p ((item progress-bar-dialog-item)
                                           determinate-p)
  (setf determinate-p (and determinate-p T)) ; ensure boolean to avoid eql problems
  (unless (eql determinate-p (progress-bar-determinate-p item))
    (setf (slot-value item 'determinate-p) determinate-p)
    (set-control-data item
                      #$kControlProgressBarIndeterminateTag
                      :byte
                      (if determinate-p 0 1)))
  determinate-p)

) ; end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MULTI PANE VIEW

;; Multi-pane-view and Tab-Bar-View are in the Appearance-Manager folder of the Examples of MCL 4.3.5 and later.
;; The following updates these modules to Carbon if they are already loaded.

#+carbon-compat
(when (module-loaded-p :tab-bar-view)

(defmethod initialize-instance ((view multi-pane-view)
                                &rest initargs
                                &key
                                (tab-font :system-font)
                                (tab-bar-height 20))
  (declare (ignore initargs))
  (call-next-method)
  (let ((tab-bar (make-instance 'tab-bar-view
                   :view-position #@(0 0)
                   :view-size     (make-point (point-h (view-size view))
                                              tab-bar-height)
                   :view-font      tab-font)))
    (setf (tab-bar view) tab-bar)
    (set-view-container tab-bar view)))

(defun tab-view-size (&key
                      view 
                      (text (dialog-item-text view))
                      (font (view-font view)))
  (multiple-value-bind (ff ms) (font-codes font)
    #-carbon-compat
    (make-point (+ (font-codes-string-width text ff ms)
                   (* 2 (if (osx-p) 12 (dialog-item-width-correction view))))
                (+ (font-codes-line-height ff ms) (if (osx-p) 4 2)))
    #+carbon-compat
    (with-font-codes ff ms
      (rlet ((size :point)
             (baseline :signed-word))
        (with-cfstrs ((cftext text)) 
          (#_GetThemeTextDimensions cftext 
           #$kThemeCurrentPortFont 
           (appearance-theme-state view)
           NIL
           size
           baseline))
        (add-points (%get-point size)
                    (make-point 
                     24 ; (* 2 (if (osx-p) 12 (dialog-item-width-correction view)))
                     (if (osx-p) 4 2)))))))
  
(add-pascal-upp-alist 'theme-tab-title-proc 
                      #'(lambda (procptr)(#_NewThemeTabTitleDrawUPP procptr)))

(defpascal theme-tab-title-proc ((:ptr :rect) bounds 
                                 :ThemeTabStyle style
                                 :ThemeTabDirection direction
                                 :word depth
                                 :Boolean isColorDev
                                 :integer userData)
  (declare (ignore style direction direction  userData))
  ; Text color is set by the OS before the call:
  (declare (ignore depth isColorDev))
  (assert *current-view*)
  (with-cfstrs ((cftext (dialog-item-text *current-view*)))
    (let* ((draw-active (window-active-p (view-window *current-view*)))
           (font-height
            (rlet ((size :point)
                   (baseline :signed-word))
              (#_GetThemeTextDimensions cftext 
               #$kThemeCurrentPortFont 
               (if draw-active
                 #$Kthemestateactive
                 #$Kthemestateinactive) 
               NIL
               size
               baseline)
              (point-v (%get-point size))))
          (height (- (rref bounds rect.bottom)
                     (rref bounds rect.top))))
      (rlet ((rect :rect))
        (copy-record bounds :rect rect)
        (#_insetRect rect 0 #-carbon-compat (ash (- height font-height) -1)
         #+carbon-compat 
         (if (osx-p) 
           (ash (- height font-height) -1)
           -1))
        (#_Drawthemetextbox cftext 
         #$kThemeCurrentPortFont 
         (if draw-active
           #$Kthemestateactive
           #$Kthemestateinactive) 
         t 
         rect 
         #$teCenter
         *null-ptr*)))))

;; # should be integrated in inner method...

(defmethod view-draw-tab :around ((view tab-view) &optional hilite-p (active-p t))
  #-carbon-compat
  (call-next-method)
  #+carbon-compat
  (with-focused-dialog-item (view)
    (with-item-rect (rect view)
      ; (#_eraseRect rect)
      (let ((*current-view* view))
      (#_DrawThemeTab rect 
       (if (selected-p view)
         (if active-p
           #$kThemeTabFront
           #$kThemeTabFrontInactive)
         (if active-p
           (if hilite-p
             #$kThemeTabNonFrontPressed
             #$kThemeTabNonFront)
           #$kThemeTabNonFrontInactive))
       #$kThemeTabNorth 
       theme-tab-title-proc
       (if hilite-p 1 0))))))

(defmethod view-activate-event-handler :after ((view tab-view))
  (invalidate-view view t))

(defmethod view-deactivate-event-handler :after ((view tab-view))
  (invalidate-view view t))

(defmethod view-click-event-handler ((view tab-view) where)
  (declare (ignore where))
  (when (and (not (selected-p view))
             (track-button-click view))
    (call-next-method)))

(defmethod view-contains-point-p ((view tab-view) point)
  #+carbon-compat
  (with-item-rect (rect view)
    (with-temp-rgns (rgn) 
      (#_GetThemeTabRegion rect #$kThemeTabFront #$kThemeTabNorth rgn)
      (#_PtInRgn point rgn)))
  #-carbon-compat
  (call-next-method))

) ; end changes to tab-bar-view

#+carbon-compat
(when (module-loaded-p :multi-pane-view)

(defmethod view-draw-contents ((view multi-pane-view))
  #+carbon-compat
  (with-back-color (or #+(and ccl-4.3.5 (not ccl-5.0)) *red-color*)
    (rlet ((rect :rect 
                   :top (1- (point-v (view-size (tab-bar view))))
                   :left 0
                   :bottomright  (subtract-points (view-size view) #@(1 1))))
        (#_DrawThemeTabPane rect 
         (if (window-active-p (view-window view))
           #$kThemeStateActive 
           #$kThemeStateInactive))
      #+carbon-compat
      (unless (osx-p) ;; perhaps even needed under osx!
        (#_applyThemeBackground #$kThemeBackgroundTabPane rect
         (if (window-active-p (view-window view))
           #$kThemeStateActive 
           #$kThemeStateInactive)
         (view-pixel-depth view) (view-color-p view))))
      (call-next-method))
    #-carbon-compat
   (progn
    (call-next-method)
    (let ((top (point-v (view-size (tab-bar view))))
          (botright (subtract-points (view-size view) #@(1 1))))
      (with-fore-color *black-color*
        (#_MoveTo :word 0 :word top)
        (#_LineTo :word 0 :word (point-v botright))
        (#_LineTo :long botright)
        (#_LineTo :word (point-h botright) :word top))
      (with-fore-color *white-color*
        (#_MoveTo :word 1 :word top)
        (#_LineTo :word 1 :word (1- (point-v botright))))
      (with-fore-color +shadow-color+
        (#_LineTo :long (subtract-points botright #@(1 1)))
        (#_LineTo :word (1- (point-h botright)) :word top)))))

(defmethod view-setup-background ((view multi-pane-view) depth color-p)
  (with-item-rect (rect view)
    (#_insetRect rect 1 1)
    (#_ApplyThemeBackground #$kThemeBackgroundTabPane rect (if (draw-active-p view) #$kThemeStateActive  #$kThemeStateInactive) Depth color-p)
    T))

(defmethod view-corners ((item multi-pane-view))
  (if (osx-p)
    (multiple-value-bind (topleft bottomright)
                         (call-next-method)
      (values
       (subtract-points topleft #@(5 0))
       bottomright))
    (call-next-method)))

(defmethod view-activate-event-handler :after ((view multi-pane-view))
  (invalidate-view view t))

(defmethod view-deactivate-event-handler :after ((view multi-pane-view))
  (invalidate-view view t))

(defmethod view-draw-contents ((view tab-bar-view))
  (call-next-method)
  #|#-carbon-compat
  (unless (appearance-version 1.1)
    (let* ((view-size (view-size view))
           (line-v (1- (point-v view-size))))
      (#_MoveTo :word 0 :word line-v)
      (#_LineTo :word (1- +initial-tab-h+) :word line-v)
      (#_MoveTo :word (max-tab-h view) :word line-v)
      (#_LineTo :word (1- (point-h view-size)) :word line-v))) |#)

) ; end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SEPARATOR DIALOG ITEM

;; Separator-Dialog-Item is in the Appearance-Manager folder of the Examples of MCL 4.3.5 and later.
;; The following updates this module to Carbon if it already is loaded.

#+carbon-compat
(when (module-loaded-p :separator-dialog-item)

(defmethod view-activate-event-handler :after ((item separator-dialog-item))
  (invalidate-view item #+carbon-compat (osx-p)))

(defmethod view-deactivate-event-handler :after ((item separator-dialog-item))
  (invalidate-view item #+carbon-compat (osx-p)))

(defmethod view-draw-contents ((item separator-dialog-item))
  (declare (function appearance-available-p))
  (if #+carbon-compat T #-carbon-compat (appearance-available-p)
    (with-item-rect (rect item)
      (#_DrawThemeSeparator rect
        (if (window-active-p (view-window item))
          1 ; $kThemeStateActive
          0 ; $kThemeStateDisabled
        )))
    #-carbon-compat
    (let* ((active? (window-active-p (view-window item)) )
           (position (+ (view-position item) #@(0 1)))
           (size (view-size item))
           (bottomright
             (if (< (point-v size)(point-h size))
               (make-point (+ (point-h size) (point-h position) -1) (point-v position))
               (make-point (point-h position) (+ (point-v size) (point-v position) -1)))))
       (with-fore-color (part-color item (if active? :separator-color :disabled-color))
         (#_MoveTo :long position)
         (#_LineTo :long bottomright))
       (with-fore-color (part-color item (if active? :hilite-color :disabled-color))
         (#_MoveTo :long (+ position #@(1 1)))
         (#_LineTo :long (+ bottomright #@(1 1)))))))

) ; end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PLACARD DIALOG ITEM

;; Placard-Dialog-Item is in the Appearance-Manager folder of the Examples of MCL 4.3.5 and later.
;; The following makes it use theme text if it already is loaded.

#+carbon-compat
(when (module-loaded-p :placard-dialog-item)

(defclass placard-dialog-item (new-control-dialog-item)
  ((procid :allocation :class :initform #$kControlPlacardProc)
   (menu :initform NIL :initarg :menu))
  (:default-initargs
   :view-font #-carbon-compat NIL #+carbon-compat (if (osx-p) :label-font :small-System-Font) ;; osx guidelines says placards should use either the small system font or the label font for text.
   :view-position #@(0 0)
   :view-size #@(64 16))) ; osx guidelines says placards should be 15 pixels high.

#+carbon-compat
(defmethod view-draw-contents ((item placard-dialog-item))
  (call-next-method)
  (with-font-focused-view item
    (with-fore-color (if (draw-active-p item)
                       *black-color* *gray-color*)
      (if (appearance-available-p)
        (let ((fill 0)) ;(floor (- (point-v (view-size item)) (font-line-height)) 2)))
          (rlet ((rect :rect 
                       :topleft (make-point 0 fill) ; (make-point 0  2))
                       :bottomright (subtract-points (view-size item) (make-point 0 fill))))
            (with-cfstrs ((cftext (dialog-item-text item)))
                            (#_SetThemeTextColor (if (draw-active-p item) 
                                                                        #$kThemeTextColorPlacardActive 
                                                                        #$kThemeTextColorPlacardInactive)
                              (view-pixel-depth item) (view-color-p item))
              (#_Drawthemetextbox cftext #$kThemeCurrentPortFont (appearance-theme-state item) t rect #$tejustcenter *null-ptr*))))
        #-carbon-compat
        (with-cstrs ((text (dialog-item-text item)))
          (when text
            (let ((length (length (dialog-item-text item))))
              (#_MoveTo 4 11)
              (#_DrawText text 0 length))))))))

) ; end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SIMPLE DEMO

#|

(setq *draw-inactive-dialog-items-as-disabled* T)

(make-instance 'window
  :theme-background T
  :back-color *lighter-gray-color*
  :view-subviews
    (list
      (make-dialog-item 'editable-text-dialog-item  #@(10 35) #@(100 14) "Editable Text")
      (make-dialog-item 'static-text-dialog-item #@(10 10) #@(300 16) "This is a demonstration of Appearance")
      (make-dialog-item 'sequence-dialog-item  #@(150 35) #@(90 80) "Sequence" NIL
        :table-sequence '("table" "dialog" "item" "alpha" "beta" 
           "gamma" "delta" "table" "dialog" "item" "alpha" "beta" "gamma" "delta"))
      (make-dialog-item 'arrow-dialog-item  #@(250 35) #@(90 80) "Arrow" NIL
        :table-vscrollp T
        :table-sequence '("arrow" "dialog" "item" "alpha" "beta" 
           "gamma" "delta" "table" "dialog" "item" "alpha" "beta" "gamma" "delta"))
      (make-dialog-item 'scrolling-fred-view  #@(10 65) #@(100 70) "Scrolling Fred")
))

#-carbon-compat
(setf *appearance-available-p* (not *appearance-available-p*))

|#

;;;-----------------------
(provide :appearance-mcl)