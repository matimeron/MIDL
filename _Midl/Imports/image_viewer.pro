;+
; NAME:
;       IMAGE_VIEWER
;
; PURPOSE:
;       The purpose of this program is to provide an interactive tool that can be used
;       to view JPEG, BMP, GIF, PNG, and TIFF picture files.  Images are loaded into
;       memory, so the initial file access may take a while, but once each picture has
;       been opened they can all be viewed in a very rapid fashion.
;
; CATEGORY:
;       Visualization, Access, Widgets
;
; CALLING SEQUENCE:
;       image_viewer
;
; INPUT PARAMETERS:
;       None.
;
; KEYWORD PARAMETERS:
;       None.
;
; OUTPUTS:
;       None.
;
; COMMON BLOCKS:
;       None.
;
; SIDE EFFECTS:
;       While this program is running in an IDL session it will change the current
;       working directory, enables/disables color decomposition, and sets !QUIET=1,
;       !ORDER=0, & !P.BACKGROUND=0.  These settings are returned to their initial
;       settings before the program was initiated once it is terminated.
;
; RESTRICTIONS:
;       This program is supported in IDL version 5.5 and newer.  In order to open
;       GIF files or TIFF files with LZW compression the copy of IDL being used must
;       be licensed with these features.  IDL only supports BMP files in the standard
;       Windows format and does not support OS2 bitmaps.
;
; MODIFICATION HISTORY:
;       Written by: AEB, 1/02.
;       Added progress bar and "Cancel" button to Status Report dialog: AEB, 2/02.
;       Modified to allow user to select screen size of program upon launch, took into
;            account Motif widget sizing on UNIX, and reduced minimum screen area
;            resolution to 800 x 600: AEB, 1/03.
;-


;*********************************************************************************************
PRO IMAGE_VIEWER_OPEN_FILES,event
;THIS PROCEDURE IS CALLED WHEN A USER SELECTS "File > Open Picture Files" FROM THE MAIN MENU
;error handling:
!ERROR_STATE.CODE=0
CATCH,error
if error NE 0 then begin
  HELP,/LAST_MESSAGE,OUTPUT=traceback
  messageStr=['Error Caught :','',traceback]
  dummy=DIALOG_MESSAGE(messageStr,/ERROR)
  ;if status report dialog is still active, destroy it:
  if SIZE(tlb,/TYPE) NE 0 then WIDGET_CONTROL,tlb,/DESTROY
  RETURN
endif
;obtain state structure for top-level-base from its UVALUE:
WIDGET_CONTROL,event.top,GET_UVALUE=pState
;prompt user to select files with native file selection dialog:
if (*pState).gifFlag EQ 1 then filter=['*.JPG','*.jpg','*.JPEG','*.jpeg','*.JPE','*.jpe',$
  '*.JFIF','*.jfif','*.GIF','*.gif','*.BMP','*.bmp','*.TIF','*.tif','*.TIFF','*.tiff',$
  '*.PNG','*.png'] else $
  filter=['*.JPG','*.jpg','*.JPEG','*.jpeg','*.JPE','*.jpe','*.JFIF','*.jfif','*.BMP','*.bmp',$
  '*.TIF','*.tif','*.TIFF','*.tiff','*.PNG','*.png']
files=DIALOG_PICKFILE(TITLE='Select picture files to open',/MULTIPLE_FILES,$
                      FILTER=filter,GET_PATH=path)
;if user hit "Cancel" then return to previous program level:
if (files[0] EQ '') then RETURN
;change current working directory to location of selected files:
CD,path
nFiles=N_ELEMENTS(files)
(*pState).nFiles=nFiles
(*pState).increment=100./nFiles
files=files[SORT(files)]
;create status report dialog:
xCenter=(*pState).screenSize[0]/2
yCenter=(*pState).screenSize[1]/2
tlb2=WIDGET_BASE(TITLE='Status Report',/COLUMN,/ALIGN_CENTER,TLB_FRAME_ATTR=19,/MODAL,$
                 GROUP_LEADER=(*pState).tlb)
  spacer=WIDGET_LABEL(tlb2,VALUE=' ')
  label1=WIDGET_LABEL(tlb2,VALUE='LOADING SELECTED IMAGE FILES INTO MEMORY')
  spacer=WIDGET_LABEL(tlb2,VALUE=' ')
  label2=WIDGET_LABEL(tlb2,VALUE='******  PLEASE WAIT  ******')
  spacer=WIDGET_LABEL(tlb2,VALUE=' ')
  statusBase=WIDGET_BASE(tlb2,/ROW,/FRAME,/BASE_ALIGN_CENTER,/ALIGN_CENTER,EVENT_PRO='image_viewer_timer')
    cancelBut=WIDGET_BUTTON(statusBase,VALUE='Cancel',EVENT_PRO='image_viewer_cancel')
    progressLabel=WIDGET_LABEL(statusBase,Value=' Progress :  0 ')
    statusSlider=WIDGET_SLIDER(statusBase,SENSITIVE=0,TITLE=' ',XSIZE=200)
    percentLabel=WIDGET_LABEL(statusBase,VALUE=' 100 %')
geom=WIDGET_INFO(tlb2,/GEOMETRY)
xHalfSize=geom.Scr_XSize/2
yHalfSize=geom.Scr_YSize/2
WIDGET_CONTROL,tlb2,XOFFSET=xCenter-xHalfSize,YOFFSET=yCenter-yHalfSize
WIDGET_CONTROL,tlb2,/REALIZE
(*pState).statusBase=statusBase
(*pState).statusSlider=statusSlider
WIDGET_CONTROL,tlb2,SET_UVALUE=pState
;reset settings of GUI:
WIDGET_CONTROL,(*pState).fileText,SET_VALUE=''
WIDGET_CONTROL,(*pState).imageDraw,GET_VALUE=drawID
WSET,drawID
TVLCT,0,0,0,0
ERASE
;re-create thumbnails base with appropriate size for number of images selected:
nRows = CEIL (nFiles / 3.0)
WIDGET_CONTROL,(*pState).thumbBase,/DESTROY
(*pState).thumbBase=WIDGET_BASE((*pState).controlsBase,/COLUMN,/ALIGN_TOP,/FRAME,XSIZE=260,$
                                YSIZE=(nRows*89),/SCROLL,X_SCROLL_SIZE=260,Y_SCROLL_SIZE=(*pState).thumbYSize)
;initialize pointer array to reference image data:
numImages=N_ELEMENTS(*(*pState).images)
if numImages NE 0 then PTR_FREE,*(*pState).images
*(*pState).images=PTRARR(nFiles,/ALLOCATE_HEAP)
*(*pState).files=files
;loop through each file:
(*pState).timer=1B
WIDGET_CONTROL,statusBase,TIMER=0.01
END
;*********************************************************************************************


;*********************************************************************************************
PRO IMAGE_VIEWER_OPEN_FOLDER,event
;THIS PROCEDURE IS CALLED WHEN A USER SELECTS "File > Open All In Folder" FROM THE MAIN MENU
;error handling:
!ERROR_STATE.CODE=0
CATCH,error
if error NE 0 then begin
  HELP,/LAST_MESSAGE,OUTPUT=traceback
  messageStr=['Error Caught :','',traceback]
  dummy=DIALOG_MESSAGE(messageStr,/ERROR)
  ;if status report dialog is still active, destroy it:
  if SIZE(tlb,/TYPE) NE 0 then WIDGET_CONTROL,tlb,/DESTROY
  RETURN
endif
;obtain state structure for top-level-base from its UVALUE:
WIDGET_CONTROL,event.top,GET_UVALUE=pState
;prompt user to select files with native file selection dialog:
folder=DIALOG_PICKFILE(TITLE='Select folder that contains picture files',/DIRECTORY)
;if user hit "Cancel" then return to previous program level:
if folder EQ '' then RETURN
;change current working directory to location of selected files:
CD,folder
if (*pState).gifFlag EQ 1 then filter=['*.JPG','*.JPEG','*.JPE','*.JFIF','*.GIF','*.BMP',$
  '*.TIF','*.TIFF','*.PNG'] else $
  filter=['*.JPG','*.JPEG','*.JPE','*.JFIF','*.BMP','*.TIF','*.TIFF','*.PNG']
files=FILE_SEARCH(filter,COUNT=nFiles,/FOLD_CASE,/FULLY_QUALIFY_PATH,/NOSORT)
if nFiles EQ 0 then begin
  dummy=DIALOG_MESSAGE('No valid picture files were found in the selected folder !',/INFO)
  RETURN
endif
(*pState).nFiles=nFiles
(*pState).increment=100./nFiles
files=files[SORT(files)]
;create status report dialog:
xCenter=(*pState).screenSize[0]/2
yCenter=(*pState).screenSize[1]/2
tlb2=WIDGET_BASE(TITLE='Status Report',/COLUMN,/ALIGN_CENTER,TLB_FRAME_ATTR=19,/MODAL,$
                 GROUP_LEADER=(*pState).tlb)
  spacer=WIDGET_LABEL(tlb2,VALUE=' ')
  label1=WIDGET_LABEL(tlb2,VALUE='LOADING SELECTED IMAGE FILES INTO MEMORY')
  spacer=WIDGET_LABEL(tlb2,VALUE=' ')
  label2=WIDGET_LABEL(tlb2,VALUE='******  PLEASE WAIT  ******')
  spacer=WIDGET_LABEL(tlb2,VALUE=' ')
  statusBase=WIDGET_BASE(tlb2,/ROW,/FRAME,/BASE_ALIGN_CENTER,/ALIGN_CENTER,EVENT_PRO='image_viewer_timer')
    cancelBut=WIDGET_BUTTON(statusBase,VALUE='Cancel',EVENT_PRO='image_viewer_cancel')
    progressLabel=WIDGET_LABEL(statusBase,Value=' Progress :  0 ')
    statusSlider=WIDGET_SLIDER(statusBase,SENSITIVE=0,TITLE=' ',XSIZE=200)
    percentLabel=WIDGET_LABEL(statusBase,VALUE=' 100 %')
geom=WIDGET_INFO(tlb2,/GEOMETRY)
xHalfSize=geom.Scr_XSize/2
yHalfSize=geom.Scr_YSize/2
WIDGET_CONTROL,tlb2,XOFFSET=xCenter-xHalfSize,YOFFSET=yCenter-yHalfSize
WIDGET_CONTROL,tlb2,/REALIZE
(*pState).statusBase=statusBase
(*pState).statusSlider=statusSlider
WIDGET_CONTROL,tlb2,SET_UVALUE=pState
;reset settings of GUI:
WIDGET_CONTROL,(*pState).fileText,SET_VALUE=''
WIDGET_CONTROL,(*pState).imageDraw,GET_VALUE=drawID
WSET,drawID
TVLCT,0,0,0,0
ERASE
;re-create thumbnails base with appropriate size for number of images selected:
nRows = CEIL (nFiles / 3.0)
WIDGET_CONTROL,(*pState).thumbBase,/DESTROY
(*pState).thumbBase=WIDGET_BASE((*pState).controlsBase,/COLUMN,/ALIGN_TOP,/FRAME,XSIZE=260,$
                                YSIZE=(nRows*89),/SCROLL,X_SCROLL_SIZE=260,Y_SCROLL_SIZE=(*pState).thumbYSize)
;initialize pointer array to reference image data:
numImages=N_ELEMENTS(*(*pState).images)
if numImages NE 0 then PTR_FREE,*(*pState).images
*(*pState).images=PTRARR(nFiles,/ALLOCATE_HEAP)
*(*pState).files=files
;loop through each file:
(*pState).timer=1B
WIDGET_CONTROL,statusBase,TIMER=0.01
END
;*********************************************************************************************


;*********************************************************************************************
PRO IMAGE_VIEWER_CANCEL,event
;obtain state structure for top-level-base from its UVALUE:
WIDGET_CONTROL,event.top,GET_UVALUE=pState
;shut-off timer:
(*pState).timer=0B
END
;*********************************************************************************************


;*********************************************************************************************
PRO IMAGE_VIEWER_TIMER,event
;obtain state structure for top-level-base from its UVALUE:
WIDGET_CONTROL,event.top,GET_UVALUE=pState
if (*pState).timer EQ 1 then begin ;continue processing files:
  if (*pState).currFile LE (*pState).nFiles-1 then begin
    i=(*pState).currFile
    extension=STRUPCASE(STRMID((*(*pState).files)[i],STRPOS((*(*pState).files)[i],'.',/REVERSE_SEARCH)+1))
    if extension EQ 'JPG' or extension EQ 'JPEG' or extension EQ 'JPE' or extension EQ 'JFIF' then begin
      result=QUERY_JPEG((*(*pState).files)[i],info)
      if result NE 1 then begin
        dummy=DIALOG_MESSAGE(['Selected file:','',(*(*pState).files)[i],'',$
                              'does not appear to be a valid JPEG file !'],/ERROR)
        if (i MOD 3) EQ 0 then (*pState).rowBase=WIDGET_BASE((*pState).thumbBase,/ROW,/ALIGN_LEFT)
        if (*pState).currFile EQ (*pState).nFiles-1 then begin
          ;last file ... terminate timer:
          (*pState).timer=0B
          (*pState).currFile=0L
          WIDGET_CONTROL,event.top,/DESTROY
        endif else begin
          ;increment file number and update progress slider:
          (*pState).currFile=(*pState).currFile+1
          progressValue = ROUND((i+1)*(*pState).increment) < 100
          WIDGET_CONTROL,(*pState).statusSlider,SET_VALUE=progressValue
          ;fire off timer again:
          WIDGET_CONTROL,(*pState).statusBase,TIMER=0.01
        endelse
        RETURN
      endif
      if (i MOD 3) EQ 0 then (*pState).rowBase=WIDGET_BASE((*pState).thumbBase,/ROW,/ALIGN_LEFT)
      TVLCT,0,0,0,0
      thumbDraw=WIDGET_DRAW((*pState).rowBase,/BUTTON_EVENTS,RETAIN=2,XSIZE=80,YSIZE=80,$
                            EVENT_PRO='image_viewer_thumbs',UVALUE=(i+1),UNAME=STRTRIM(i+1,2))
      WAIT,0.01
      WIDGET_CONTROL,thumbDraw,GET_VALUE=drawID
      WSET,drawID
      if info.channels EQ 3 then begin
        READ_JPEG,(*(*pState).files)[i],image,TRUE=1
        if (*pState).colorMode EQ 'PSEUDO' then begin
          image=COLOR_QUAN(TEMPORARY(image),1,red,green,blue,COLORS=!D.TABLE_SIZE-1)
          red=[[0],TEMPORARY(red)]
          green=[[0],TEMPORARY(green)]
          blue=[[0],TEMPORARY(blue)]
          image=TEMPORARY(image)+1B
          imageColorMode='PSEUDO'
          TVLCT,red,green,blue
        endif else begin
          imageColorMode='TRUE'
          red=0B
          green=0B
          blue=0B
          DEVICE,DECOMPOSED=1
        endelse
      endif
      if info.channels EQ 1 then begin
        READ_JPEG,(*(*pState).files)[i],image
        if (*pState).colorMode EQ 'PSEUDO' then image=BYTSCL(TEMPORARY(image),TOP=!D.TABLE_SIZE-1)
        red=BINDGEN(!D.TABLE_SIZE)
        green=BINDGEN(!D.TABLE_SIZE)
        blue=BINDGEN(!D.TABLE_SIZE)
        imageColorMode='PSEUDO'
        if (*pState).colorMode EQ 'TRUE' then DEVICE,DECOMPOSED=0
        TVLCT,red,green,blue
      endif
      ratio=FLOAT(info.dimensions[0])/info.dimensions[1]
      ;resize image if necessary:
      if info.dimensions[0] GT (*pState).drawXSize or info.dimensions[1] GT (*pState).drawYSize then begin
        if ratio GE 1.09231 then begin
          factor=(DOUBLE((*pState).drawXSize)/info.dimensions[0])
          xSize=(*pState).drawXSize
          ySize=ROUND(info.dimensions[1]*factor)
          if imageColorMode EQ 'PSEUDO' then image=CONGRID(image,(*pState).drawXSize,ySize)
          if imageColorMode EQ 'TRUE' then image=CONGRID(image,3,(*pState).drawXSize,ySize)
        endif else begin
          factor=(DOUBLE((*pState).drawYSize)/info.dimensions[1])
          xSize=ROUND(info.dimensions[0]*factor)
          ySize=(*pState).drawYSize
          if imageColorMode EQ 'PSEUDO' then image=CONGRID(image,xSize,(*pState).drawYSize)
          if imageColorMode EQ 'TRUE' then image=CONGRID(image,3,xSize,(*pState).drawYSize)
        endelse
      endif else begin
        xSize=info.dimensions[0]
        ySize=info.dimensions[1]
      endelse
      ;create thumbnail:
      if xSize GT 80 or ySize GT 80 then begin
        if ratio GE 1.09231 then begin
          factor=(80./info.dimensions[0])
          thumbxSize=80
          thumbySize=ROUND(info.dimensions[1]*factor)
          if imageColorMode EQ 'PSEUDO' then thumb=CONGRID(image,80,thumbySize)
          if imageColorMode EQ 'TRUE' then thumb=CONGRID(image,3,80,thumbySize)
        endif else begin
          factor=(80./info.dimensions[1])
          thumbxSize=ROUND(info.dimensions[0]*factor)
          thumbySize=80
          if imageColorMode EQ 'PSEUDO' then thumb=CONGRID(image,thumbxSize,80)
          if imageColorMode EQ 'TRUE' then thumb=CONGRID(image,3,thumbxSize,80)
        endelse
        xOffset=ROUND((80-thumbxSize)/2.)
        yOffset=ROUND((80-thumbySize)/2.)
      endif else begin
        bottom=FLOOR((80-ySize)/2.)
        top=bottom+ySize-1
        left=FLOOR((80-xSize)/2.)
        right=left+xSize-1
        if imageColorMode EQ 'PSEUDO' then begin
          thumb=BYTARR(80,80)
          thumb[left:right,bottom:top]=image
        endif else begin ;imageColorMode EQ 'TRUE':
          thumb=BYTARR(3,80,80)
          thumb[*,left:right,bottom:top]=image
        endelse
        xOffset=0
        yOffset=0
      endelse
      if imageColorMode EQ 'PSEUDO' then TV,TEMPORARY(thumb),xOffset,yOffset
      if imageColorMode EQ 'TRUE' then TV,TEMPORARY(thumb),xOffset,yOffset,TRUE=1
      imageStruct={image:TEMPORARY(image),xSize:xSize,ySize:ySize,imageColorMode:imageColorMode,$
                   red:TEMPORARY(red),green:TEMPORARY(green),blue:TEMPORARY(blue)}
      *(*(*pState).images)[i]=TEMPORARY(imageStruct)
    endif
    ;
    if extension EQ 'GIF' and (*pState).gifFlag EQ 1 then begin
      result=QUERY_GIF((*(*pState).files)[i],info)
      if result NE 1 then begin
        dummy=DIALOG_MESSAGE(['An error has occurred.  This is due to one of the following reasons :','',$
                              '1) This installation of IDL is not licensed for GIF technology patented',$
                              'by the Unisys Corporation.','',$
                              '2) Selected file:','',(*(*pState).files)[i],'',$
                              'is not a valid GIF file.'],/ERROR)
        if (i MOD 3) EQ 0 then (*pState).rowBase=WIDGET_BASE((*pState).thumbBase,/ROW,/ALIGN_LEFT)
        if (*pState).currFile EQ (*pState).nFiles-1 then begin
          ;last file ... terminate timer:
          (*pState).timer=0B
          (*pState).currFile=0L
          WIDGET_CONTROL,event.top,/DESTROY
        endif else begin
          ;increment file number and update progress slider:
          (*pState).currFile=(*pState).currFile+1
          progressValue = ROUND((i+1)*(*pState).increment) < 100
          WIDGET_CONTROL,(*pState).statusSlider,SET_VALUE=progressValue
          ;fire off timer again:
          WIDGET_CONTROL,(*pState).statusBase,TIMER=0.01
        endelse
        RETURN
      endif
      if (i MOD 3) EQ 0 then (*pState).rowBase=WIDGET_BASE((*pState).thumbBase,/ROW,/ALIGN_LEFT)
      TVLCT,0,0,0,0
      thumbDraw=WIDGET_DRAW((*pState).rowBase,/BUTTON_EVENTS,RETAIN=2,XSIZE=80,YSIZE=80,$
                            EVENT_PRO='image_viewer_thumbs',UVALUE=(i+1),UNAME=STRTRIM(i+1,2))
      WAIT,0.01
      WIDGET_CONTROL,thumbDraw,GET_VALUE=drawID
      WSET,drawID
      if info.has_palette EQ 1 then begin
        READ_GIF,(*(*pState).files)[i],image,red,green,blue
        trueColorImage=BYTARR(3,info.dimensions[0],info.dimensions[1])
        trueColorImage[0,*,*]=red[image]
        trueColorImage[1,*,*]=green[image]
        trueColorImage[2,*,*]=blue[image]
        image=COLOR_QUAN(TEMPORARY(trueColorImage),1,red,green,blue,COLORS=!D.TABLE_SIZE-1)
        red=[[0],TEMPORARY(red)]
        green=[[0],TEMPORARY(green)]
        blue=[[0],TEMPORARY(blue)]
        image=TEMPORARY(image)+1B
      endif
      if info.has_palette EQ 0 then begin
        READ_GIF,(*(*pState).files)[i],image
        if (*pState).colorMode EQ 'PSEUDO' then image=BYTSCL(TEMPORARY(image),TOP=!D.TABLE_SIZE-1)
        red=BINDGEN(!D.TABLE_SIZE)
        green=BINDGEN(!D.TABLE_SIZE)
        blue=BINDGEN(!D.TABLE_SIZE)
      endif
      if (*pState).colorMode EQ 'TRUE' then DEVICE,DECOMPOSED=0
      TVLCT,red,green,blue
      imageColorMode='PSEUDO'
      ratio=FLOAT(info.dimensions[0])/info.dimensions[1]
      ;resize image if necessary:
      if info.dimensions[0] GT (*pState).drawXSize or info.dimensions[1] GT (*pState).drawYSize then begin
        if ratio GE 1.09231 then begin
          factor=(DOUBLE((*pState).drawXSize)/info.dimensions[0])
          xSize=(*pState).drawXSize
          ySize=ROUND(info.dimensions[1]*factor)
          image=CONGRID(image,(*pState).drawXSize,ySize)
        endif else begin
          factor=(DOUBLE((*pState).drawYSize)/info.dimensions[1])
          xSize=ROUND(info.dimensions[0]*factor)
          ySize=(*pState).drawYSize
          image=CONGRID(image,xSize,(*pState).drawYSize)
        endelse
      endif else begin
        xSize=info.dimensions[0]
        ySize=info.dimensions[1]
      endelse
      ;create thumbnail:
      if xSize GT 80 or ySize GT 80 then begin
        if ratio GE 1.09231 then begin
          factor=(80./info.dimensions[0])
          thumbxSize=80
          thumbySize=ROUND(info.dimensions[1]*factor)
          thumb=CONGRID(image,80,thumbySize)
        endif else begin
          factor=(80./info.dimensions[1])
          thumbxSize=ROUND(info.dimensions[0]*factor)
          thumbySize=80
          thumb=CONGRID(image,thumbxSize,80)
        endelse
        xOffset=ROUND((80-thumbxSize)/2.)
        yOffset=ROUND((80-thumbySize)/2.)
      endif else begin
        bottom=FLOOR((80-ySize)/2.)
        top=bottom+ySize-1
        left=FLOOR((80-xSize)/2.)
        right=left+xSize-1
        thumb=BYTARR(80,80)
        thumb[left:right,bottom:top]=image
        xOffset=0
        yOffset=0
      endelse
      TV,TEMPORARY(thumb),xOffset,yOffset
      imageStruct={image:TEMPORARY(image),xSize:xSize,ySize:ySize,imageColorMode:imageColorMode,$
                   red:TEMPORARY(red),green:TEMPORARY(green),blue:TEMPORARY(blue)}
      *(*(*pState).images)[i]=TEMPORARY(imageStruct)
    endif
    ;
    if extension EQ 'BMP' then begin
      result=QUERY_BMP((*(*pState).files)[i],info)
      if result NE 1 then begin
        dummy=DIALOG_MESSAGE(['An error has occurred.  This is due to one of the following reasons :','',$
                              '1) The selected BMP file is an "OS2" format file, which is not supported',$
                              'by IMAGE_VIEWER.','',$
                              '2) Selected file:','',(*(*pState).files)[i],'',$
                              'is not a valid BMP file.'],/ERROR)
        if (i MOD 3) EQ 0 then (*pState).rowBase=WIDGET_BASE((*pState).thumbBase,/ROW,/ALIGN_LEFT)
        if (*pState).currFile EQ (*pState).nFiles-1 then begin
          ;last file ... terminate timer:
          (*pState).timer=0B
          (*pState).currFile=0L
          WIDGET_CONTROL,event.top,/DESTROY
        endif else begin
          ;increment file number and update progress slider:
          (*pState).currFile=(*pState).currFile+1
          progressValue = ROUND((i+1)*(*pState).increment) < 100
          WIDGET_CONTROL,(*pState).statusSlider,SET_VALUE=progressValue
          ;fire off timer again:
          WIDGET_CONTROL,(*pState).statusBase,TIMER=0.01
        endelse
        RETURN
      endif
      if (i MOD 3) EQ 0 then (*pState).rowBase=WIDGET_BASE((*pState).thumbBase,/ROW,/ALIGN_LEFT)
      TVLCT,0,0,0,0
      thumbDraw=WIDGET_DRAW((*pState).rowBase,/BUTTON_EVENTS,RETAIN=2,XSIZE=80,YSIZE=80,$
                            EVENT_PRO='image_viewer_thumbs',UVALUE=(i+1),UNAME=STRTRIM(i+1,2))
      WAIT,0.01
      WIDGET_CONTROL,thumbDraw,GET_VALUE=drawID
      WSET,drawID
      if info.channels EQ 3 then begin
        image=READ_BMP((*(*pState).files)[i],/RGB)
        if (*pState).colorMode EQ 'PSEUDO' then begin
          image=COLOR_QUAN(image,1,red,green,blue,COLORS=!D.TABLE_SIZE-1)
          red=[[0],TEMPORARY(red)]
          green=[[0],TEMPORARY(green)]
          blue=[[0],TEMPORARY(blue)]
          image=TEMPORARY(image)+1B
          imageColorMode='PSEUDO'
          TVLCT,red,green,blue
        endif else begin
          imageColorMode='TRUE'
          red=0B
          green=0B
          blue=0B
          DEVICE,DECOMPOSED=1
        endelse
      endif
      if info.channels EQ 1 then begin
        image=READ_BMP((*(*pState).files)[i],red,green,blue)
        if info.has_palette EQ 0 then begin
          if (*pState).colorMode EQ 'PSEUDO' then image=BYTSCL(TEMPORARY(image),TOP=!D.TABLE_SIZE-1)
          red=BINDGEN(!D.TABLE_SIZE)
          green=BINDGEN(!D.TABLE_SIZE)
          blue=BINDGEN(!D.TABLE_SIZE)
        endif else begin
          trueColorImage=BYTARR(3,info.dimensions[0],info.dimensions[1])
          trueColorImage[0,*,*]=red[image]
          trueColorImage[1,*,*]=green[image]
          trueColorImage[2,*,*]=blue[image]
          image=COLOR_QUAN(TEMPORARY(trueColorImage),1,red,green,blue,COLORS=!D.TABLE_SIZE-1)
          red=[[0],TEMPORARY(red)]
          green=[[0],TEMPORARY(green)]
          blue=[[0],TEMPORARY(blue)]
          image=TEMPORARY(image)+1B
        endelse
        imageColorMode='PSEUDO'
        if (*pState).colorMode EQ 'TRUE' then DEVICE,DECOMPOSED=0
        TVLCT,red,green,blue
      endif
      ratio=FLOAT(info.dimensions[0])/info.dimensions[1]
      ;resize image if necessary:
      if info.dimensions[0] GT (*pState).drawXSize or info.dimensions[1] GT (*pState).drawYSize then begin
        if ratio GE 1.09231 then begin
          factor=(DOUBLE((*pState).drawXSize)/info.dimensions[0])
          xSize=(*pState).drawXSize
          ySize=ROUND(info.dimensions[1]*factor)
          if imageColorMode EQ 'PSEUDO' then image=CONGRID(image,(*pState).drawXSize,ySize)
          if imageColorMode EQ 'TRUE' then image=CONGRID(image,3,(*pState).drawXSize,ySize)
        endif else begin
          factor=(DOUBLE((*pState).drawYSize)/info.dimensions[1])
          xSize=ROUND(info.dimensions[0]*factor)
          ySize=(*pState).drawYSize
          if imageColorMode EQ 'PSEUDO' then image=CONGRID(image,xSize,(*pState).drawYSize)
          if imageColorMode EQ 'TRUE' then image=CONGRID(image,3,xSize,(*pState).drawYSize)
        endelse
      endif else begin
        xSize=info.dimensions[0]
        ySize=info.dimensions[1]
      endelse
      ;create thumbnail:
      if xSize GT 80 or ySize GT 80 then begin
        if ratio GE 1.09231 then begin
          factor=(80./info.dimensions[0])
          thumbxSize=80
          thumbySize=ROUND(info.dimensions[1]*factor)
          if imageColorMode EQ 'PSEUDO' then thumb=CONGRID(image,80,thumbySize)
          if imageColorMode EQ 'TRUE' then thumb=CONGRID(image,3,80,thumbySize)
        endif else begin
          factor=(80./info.dimensions[1])
          thumbxSize=ROUND(info.dimensions[0]*factor)
          thumbySize=80
          if imageColorMode EQ 'PSEUDO' then thumb=CONGRID(image,thumbxSize,80)
          if imageColorMode EQ 'TRUE' then thumb=CONGRID(image,3,thumbxSize,80)
        endelse
        xOffset=ROUND((80-thumbxSize)/2.)
        yOffset=ROUND((80-thumbySize)/2.)
      endif else begin
        bottom=FLOOR((80-ySize)/2.)
        top=bottom+ySize-1
        left=FLOOR((80-xSize)/2.)
        right=left+xSize-1
        if imageColorMode EQ 'PSEUDO' then begin
          thumb=BYTARR(80,80)
          thumb[left:right,bottom:top]=image
        endif else begin ;imageColorMode EQ 'TRUE':
          thumb=BYTARR(3,80,80)
          thumb[*,left:right,bottom:top]=image
        endelse
        xOffset=0
        yOffset=0
      endelse
      if imageColorMode EQ 'PSEUDO' then TV,TEMPORARY(thumb),xOffset,yOffset
      if imageColorMode EQ 'TRUE' then TV,TEMPORARY(thumb),xOffset,yOffset,TRUE=1
      imageStruct={image:TEMPORARY(image),xSize:xSize,ySize:ySize,imageColorMode:imageColorMode,$
                   red:TEMPORARY(red),green:TEMPORARY(green),blue:TEMPORARY(blue)}
      *(*(*pState).images)[i]=TEMPORARY(imageStruct)
    endif
    ;
    if extension EQ 'TIF' or extension EQ 'TIFF' then begin
      result=QUERY_TIFF((*(*pState).files)[i],info)
      if result NE 1 then begin
        dummy=DIALOG_MESSAGE(['An error has occurred.  This is due to one of the following reasons :','',$
              '1) The selected TIFF file has LZW (Lempel-Zif-Welch) compression and this installation',$
              'of IDL is not licensed for TIFF LZW technology patented by the Unisys Corporation.','',$
              '2) Selected file:','',(*(*pState).files)[i],'',$
              'is not a valid TIFF file.'],/ERROR)
        if (i MOD 3) EQ 0 then (*pState).rowBase=WIDGET_BASE((*pState).thumbBase,/ROW,/ALIGN_LEFT)
        if (*pState).currFile EQ (*pState).nFiles-1 then begin
          ;last file ... terminate timer:
          (*pState).timer=0B
          (*pState).currFile=0L
          WIDGET_CONTROL,event.top,/DESTROY
        endif else begin
          ;increment file number and update progress slider:
          (*pState).currFile=(*pState).currFile+1
          progressValue = ROUND((i+1)*(*pState).increment) < 100
          WIDGET_CONTROL,(*pState).statusSlider,SET_VALUE=progressValue
          ;fire off timer again:
          WIDGET_CONTROL,(*pState).statusBase,TIMER=0.01
        endelse
        RETURN
      endif
      if info.orientation NE 0 and info.orientation NE 1 and info.orientation NE 4 then begin
        dummy=DIALOG_MESSAGE('IMAGE_VIEWER only works with standard orientation TIFF files !',/ERROR)
        if (i MOD 3) EQ 0 then (*pState).rowBase=WIDGET_BASE((*pState).thumbBase,/ROW,/ALIGN_LEFT)
        if (*pState).currFile EQ (*pState).nFiles-1 then begin
          ;last file ... terminate timer:
          (*pState).timer=0B
          (*pState).currFile=0L
          WIDGET_CONTROL,event.top,/DESTROY
        endif else begin
          ;increment file number and update progress slider:
          (*pState).currFile=(*pState).currFile+1
          progressValue = ROUND((i+1)*(*pState).increment) < 100
          WIDGET_CONTROL,(*pState).statusSlider,SET_VALUE=progressValue
          ;fire off timer again:
          WIDGET_CONTROL,(*pState).statusBase,TIMER=0.01
        endelse
        RETURN
      endif
      if (i MOD 3) EQ 0 then (*pState).rowBase=WIDGET_BASE((*pState).thumbBase,/ROW,/ALIGN_LEFT)
      TVLCT,0,0,0,0
      thumbDraw=WIDGET_DRAW((*pState).rowBase,/BUTTON_EVENTS,RETAIN=2,XSIZE=80,YSIZE=80,$
                            EVENT_PRO='image_viewer_thumbs',UVALUE=(i+1),UNAME=STRTRIM(i+1,2))
      WAIT,0.01
      WIDGET_CONTROL,thumbDraw,GET_VALUE=drawID
      WSET,drawID
      if info.channels EQ 3 then begin
        image=READ_TIFF((*(*pState).files)[i],INTERLEAVE=0,ORDER=order,IMAGE_INDEX=0)
        if info.pixel_type NE 1 then image=BYTSCL(TEMPORARY(image))
        if order EQ 1 then begin
          ;flip image:
          rImage=REFORM(image[0,*,*])
          rImage=ROTATE(rImage,7)
          gImage=REFORM(image[1,*,*])
          gImage=ROTATE(gImage,7)
          bImage=REFORM(image[2,*,*])
          bImage=ROTATE(bImage,7)
          image=BYTARR(3,info.dimensions[0],info.dimensions[1])
          image[0,*,*]=TEMPORARY(rImage)
          image[1,*,*]=TEMPORARY(gImage)
          image[2,*,*]=TEMPORARY(bImage)
        endif
        if (*pState).colorMode EQ 'PSEUDO' then begin
          image=COLOR_QUAN(image,1,red,green,blue,COLORS=!D.TABLE_SIZE-1)
          red=[[0],TEMPORARY(red)]
          green=[[0],TEMPORARY(green)]
          blue=[[0],TEMPORARY(blue)]
          image=TEMPORARY(image)+1B
          imageColorMode='PSEUDO'
          TVLCT,red,green,blue
        endif else begin
          imageColorMode='TRUE'
          red=0B
          green=0B
          blue=0B
          DEVICE,DECOMPOSED=1
        endelse
      endif
      if info.channels EQ 1 then begin
        image=READ_TIFF((*(*pState).files)[i],red,green,blue,ORDER=order,IMAGE_INDEX=0)
        if info.pixel_type NE 1 then image=BYTSCL(TEMPORARY(image))
        if order EQ 1 then begin
          ;flip image:
          image=ROTATE(TEMPORARY(image),7)
        endif
        if info.has_palette EQ 0 then begin
          if (*pState).colorMode EQ 'PSEUDO' then image=BYTSCL(TEMPORARY(image),TOP=!D.TABLE_SIZE-1)
          red=BINDGEN(!D.TABLE_SIZE)
          green=BINDGEN(!D.TABLE_SIZE)
          blue=BINDGEN(!D.TABLE_SIZE)
        endif else begin
          trueColorImage=BYTARR(3,info.dimensions[0],info.dimensions[1])
          trueColorImage[0,*,*]=red[image]
          trueColorImage[1,*,*]=green[image]
          trueColorImage[2,*,*]=blue[image]
          image=COLOR_QUAN(TEMPORARY(trueColorImage),1,red,green,blue,COLORS=!D.TABLE_SIZE-1)
          red=[[0],TEMPORARY(red)]
          green=[[0],TEMPORARY(green)]
          blue=[[0],TEMPORARY(blue)]
          image=TEMPORARY(image)+1B
        endelse
        imageColorMode='PSEUDO'
        if (*pState).colorMode EQ 'TRUE' then DEVICE,DECOMPOSED=0
        TVLCT,red,green,blue
      endif
      ratio=FLOAT(info.dimensions[0])/info.dimensions[1]
      ;resize image if necessary:
      if info.dimensions[0] GT (*pState).drawXSize or info.dimensions[1] GT (*pState).drawYSize then begin
        if ratio GE 1.09231 then begin
          factor=(DOUBLE((*pState).drawXSize)/info.dimensions[0])
          xSize=(*pState).drawXSize
          ySize=ROUND(info.dimensions[1]*factor)
          if imageColorMode EQ 'PSEUDO' then image=CONGRID(image,(*pState).drawXSize,ySize)
          if imageColorMode EQ 'TRUE' then image=CONGRID(image,3,(*pState).drawXSize,ySize)
        endif else begin
          factor=(DOUBLE((*pState).drawYSize)/info.dimensions[1])
          xSize=ROUND(info.dimensions[0]*factor)
          ySize=(*pState).drawYSize
          if imageColorMode EQ 'PSEUDO' then image=CONGRID(image,xSize,(*pState).drawYSize)
          if imageColorMode EQ 'TRUE' then image=CONGRID(image,3,xSize,(*pState).drawYSize)
        endelse
      endif else begin
        xSize=info.dimensions[0]
        ySize=info.dimensions[1]
      endelse
      ;create thumbnail:
      if xSize GT 80 or ySize GT 80 then begin
        if ratio GE 1.09231 then begin
          factor=(80./info.dimensions[0])
          thumbxSize=80
          thumbySize=ROUND(info.dimensions[1]*factor)
          if imageColorMode EQ 'PSEUDO' then thumb=CONGRID(image,80,thumbySize)
          if imageColorMode EQ 'TRUE' then thumb=CONGRID(image,3,80,thumbySize)
        endif else begin
          factor=(80./info.dimensions[1])
          thumbxSize=ROUND(info.dimensions[0]*factor)
          thumbySize=80
          if imageColorMode EQ 'PSEUDO' then thumb=CONGRID(image,thumbxSize,80)
          if imageColorMode EQ 'TRUE' then thumb=CONGRID(image,3,thumbxSize,80)
        endelse
        xOffset=ROUND((80-thumbxSize)/2.)
        yOffset=ROUND((80-thumbySize)/2.)
      endif else begin
        bottom=FLOOR((80-ySize)/2.)
        top=bottom+ySize-1
        left=FLOOR((80-xSize)/2.)
        right=left+xSize-1
        if imageColorMode EQ 'PSEUDO' then begin
          thumb=BYTARR(80,80)
          thumb[left:right,bottom:top]=image
        endif else begin ;imageColorMode EQ 'TRUE':
          thumb=BYTARR(3,80,80)
          thumb[*,left:right,bottom:top]=image
        endelse
        xOffset=0
        yOffset=0
      endelse
      if imageColorMode EQ 'PSEUDO' then TV,TEMPORARY(thumb),xOffset,yOffset
      if imageColorMode EQ 'TRUE' then TV,TEMPORARY(thumb),xOffset,yOffset,TRUE=1
      imageStruct={image:TEMPORARY(image),xSize:xSize,ySize:ySize,imageColorMode:imageColorMode,$
                   red:TEMPORARY(red),green:TEMPORARY(green),blue:TEMPORARY(blue)}
      *(*(*pState).images)[i]=TEMPORARY(imageStruct)
    endif
    ;
    if extension EQ 'PNG' then begin
      result=QUERY_PNG((*(*pState).files)[i],info)
      if result NE 1 then begin
        dummy=DIALOG_MESSAGE(['Selected file:','',(*(*pState).files)[i],'',$
                              'does not appear to be a valid PNG file !'],/ERROR)
        if (i MOD 3) EQ 0 then (*pState).rowBase=WIDGET_BASE((*pState).thumbBase,/ROW,/ALIGN_LEFT)
        if (*pState).currFile EQ (*pState).nFiles-1 then begin
          ;last file ... terminate timer:
          (*pState).timer=0B
          (*pState).currFile=0L
          WIDGET_CONTROL,event.top,/DESTROY
        endif else begin
          ;increment file number and update progress slider:
          (*pState).currFile=(*pState).currFile+1
          progressValue = ROUND((i+1)*(*pState).increment) < 100
          WIDGET_CONTROL,(*pState).statusSlider,SET_VALUE=progressValue
          ;fire off timer again:
          WIDGET_CONTROL,(*pState).statusBase,TIMER=0.01
        endelse
        RETURN
      endif
      if (i MOD 3) EQ 0 then (*pState).rowBase=WIDGET_BASE((*pState).thumbBase,/ROW,/ALIGN_LEFT)
      TVLCT,0,0,0,0
      thumbDraw=WIDGET_DRAW((*pState).rowBase,/BUTTON_EVENTS,RETAIN=2,XSIZE=80,YSIZE=80,$
                            EVENT_PRO='image_viewer_thumbs',UVALUE=(i+1),UNAME=STRTRIM(i+1,2))
      WAIT,0.01
      WIDGET_CONTROL,thumbDraw,GET_VALUE=drawID
      WSET,drawID
      if info.channels EQ 3 then begin
        image=READ_PNG((*(*pState).files)[i])
        if info.pixel_type NE 1 then image=BYTSCL(TEMPORARY(image))
        if (*pState).colorMode EQ 'PSEUDO' then begin
          image=COLOR_QUAN(image,1,red,green,blue,COLORS=!D.TABLE_SIZE-1)
          red=[[0],TEMPORARY(red)]
          green=[[0],TEMPORARY(green)]
          blue=[[0],TEMPORARY(blue)]
          image=TEMPORARY(image)+1B
          imageColorMode='PSEUDO'
          TVLCT,red,green,blue
        endif else begin
          imageColorMode='TRUE'
          red=0B
          green=0B
          blue=0B
          DEVICE,DECOMPOSED=1
        endelse
      endif
      if info.channels EQ 1 then begin
        image=READ_PNG((*(*pState).files)[i],red,green,blue)
        if info.pixel_type NE 1 then image=BYTSCL(TEMPORARY(image))
        if info.has_palette EQ 0 then begin
          if (*pState).colorMode EQ 'PSEUDO' then image=BYTSCL(TEMPORARY(image),TOP=!D.TABLE_SIZE-1)
          red=BINDGEN(!D.TABLE_SIZE)
          green=BINDGEN(!D.TABLE_SIZE)
          blue=BINDGEN(!D.TABLE_SIZE)
        endif else begin
          trueColorImage=BYTARR(3,info.dimensions[0],info.dimensions[1])
          trueColorImage[0,*,*]=red[image]
          trueColorImage[1,*,*]=green[image]
          trueColorImage[2,*,*]=blue[image]
          image=COLOR_QUAN(TEMPORARY(trueColorImage),1,red,green,blue,COLORS=!D.TABLE_SIZE-1)
          red=[[0],TEMPORARY(red)]
          green=[[0],TEMPORARY(green)]
          blue=[[0],TEMPORARY(blue)]
          image=TEMPORARY(image)+1B
        endelse
        imageColorMode='PSEUDO'
        if (*pState).colorMode EQ 'TRUE' then DEVICE,DECOMPOSED=0
        TVLCT,red,green,blue
      endif
      ratio=FLOAT(info.dimensions[0])/info.dimensions[1]
      ;resize image if necessary:
      if info.dimensions[0] GT (*pState).drawXSize or info.dimensions[1] GT (*pState).drawYSize then begin
        if ratio GE 1.09231 then begin
          factor=(DOUBLE((*pState).drawXSize)/info.dimensions[0])
          xSize=(*pState).drawXSize
          ySize=ROUND(info.dimensions[1]*factor)
          if imageColorMode EQ 'PSEUDO' then image=CONGRID(image,(*pState).drawXSize,ySize)
          if imageColorMode EQ 'TRUE' then image=CONGRID(image,3,(*pState).drawXSize,ySize)
        endif else begin
          factor=(DOUBLE((*pState).drawYSize)/info.dimensions[1])
          xSize=ROUND(info.dimensions[0]*factor)
          ySize=(*pState).drawYSize
          if imageColorMode EQ 'PSEUDO' then image=CONGRID(image,xSize,(*pState).drawYSize)
          if imageColorMode EQ 'TRUE' then image=CONGRID(image,3,xSize,(*pState).drawYSize)
        endelse
      endif else begin
        xSize=info.dimensions[0]
        ySize=info.dimensions[1]
      endelse
      ;create thumbnail:
      if xSize GT 80 or ySize GT 80 then begin
        if ratio GE 1.09231 then begin
          factor=(80./info.dimensions[0])
          thumbxSize=80
          thumbySize=ROUND(info.dimensions[1]*factor)
          if imageColorMode EQ 'PSEUDO' then thumb=CONGRID(image,80,thumbySize)
          if imageColorMode EQ 'TRUE' then thumb=CONGRID(image,3,80,thumbySize)
        endif else begin
          factor=(80./info.dimensions[1])
          thumbxSize=ROUND(info.dimensions[0]*factor)
          thumbySize=80
          if imageColorMode EQ 'PSEUDO' then thumb=CONGRID(image,thumbxSize,80)
          if imageColorMode EQ 'TRUE' then thumb=CONGRID(image,3,thumbxSize,80)
        endelse
        xOffset=ROUND((80-thumbxSize)/2.)
        yOffset=ROUND((80-thumbySize)/2.)
      endif else begin
        bottom=FLOOR((80-ySize)/2.)
        top=bottom+ySize-1
        left=FLOOR((80-xSize)/2.)
        right=left+xSize-1
        if imageColorMode EQ 'PSEUDO' then begin
          thumb=BYTARR(80,80)
          thumb[left:right,bottom:top]=image
        endif else begin ;imageColorMode EQ 'TRUE':
          thumb=BYTARR(3,80,80)
          thumb[*,left:right,bottom:top]=image
        endelse
        xOffset=0
        yOffset=0
      endelse
      if imageColorMode EQ 'PSEUDO' then TV,TEMPORARY(thumb),xOffset,yOffset
      if imageColorMode EQ 'TRUE' then TV,TEMPORARY(thumb),xOffset,yOffset,TRUE=1
      imageStruct={image:TEMPORARY(image),xSize:xSize,ySize:ySize,imageColorMode:imageColorMode,$
                   red:TEMPORARY(red),green:TEMPORARY(green),blue:TEMPORARY(blue)}
      *(*(*pState).images)[i]=TEMPORARY(imageStruct)
    endif
    ;increment file number and update progress slider:
    (*pState).currFile=(*pState).currFile+1
    progressValue = ROUND((i+1)*(*pState).increment) < 100
    WIDGET_CONTROL,(*pState).statusSlider,SET_VALUE=progressValue
    ;fire off timer again:
    WIDGET_CONTROL,(*pState).statusBase,TIMER=0.01
  endif else begin ;all files have already been read-in:
    (*pState).timer=0B
    (*pState).currFile=0L
    WIDGET_CONTROL,event.top,/DESTROY
  endelse
endif else begin ;user hit "Cancel":
  (*pState).currFile=0L
  WIDGET_CONTROL,event.top,/DESTROY
endelse
END
;*********************************************************************************************


;*********************************************************************************************
PRO IMAGE_VIEWER_EXIT,event
;THIS PROCEDURE IS CALLED WHEN A USER SELECTS "File > Exit" FROM THE MAIN MENU
;terminate the program by destroying the top-level-base (widgetID always stored in event.top):
WIDGET_CONTROL,event.top,/DESTROY
END
;*********************************************************************************************


;*********************************************************************************************
PRO IMAGE_VIEWER_HELP,event
;THIS PROCEDURE IS CALLED WHEN A USER SELECTS "Help > About IMAGE_VIEWER"
;FROM THE MAIN MENU
;display a simple message:
messageStr=['IMAGE_VIEWER','Copyright (C) 2002-2003,  AEB.','',$
            'The purpose of this program is to provide an interactive tool that can be used',$
            'to view JPEG, BMP, GIF, PNG, and TIFF picture files.  In order to provide rapid',$
            'viewing capabilities the images are loaded into memory, which can cause the',$
            'initial file access to take a bit of time while the pictures are opened and',$
            'thumbnails are created.']
dummy=DIALOG_MESSAGE(messageStr,/info)
END
;*********************************************************************************************


;*********************************************************************************************
PRO IMAGE_VIEWER_THUMBS,event
;THIS PROCEDURE IS CALLED WHEN A USER CLICKS ON ONE OF THE THUMBNAIL PICTURES
;error handling:
!ERROR_STATE.CODE=0
CATCH,error
if error NE 0 then begin
  HELP,/LAST_MESSAGE,OUTPUT=traceback
  messageStr=['Error Caught :','',traceback]
  dummy=DIALOG_MESSAGE(messageStr,/ERROR)
  RETURN
endif
if event.press EQ 1 then begin
  WIDGET_CONTROL,/HOURGLASS
  ;obtain state structure for top-level-base from its UVALUE:
  WIDGET_CONTROL,event.top,GET_UVALUE=pState
  WIDGET_CONTROL,(*pState).imageDraw,GET_VALUE=drawID
  WSET,drawID
  TVLCT,0,0,0,0
  ERASE
  ;obtain current image data:
  WIDGET_CONTROL,event.id,GET_UVALUE=fileID
  imageStruct=*(*(*pState).images)[fileID-1]
  xOffset=ROUND(((*pState).drawXSize-imageStruct.xSize)/2.)
  yOffset=ROUND(((*pState).drawYSize-imageStruct.ySize)/2.)
  if (*pState).colorMode EQ 'PSEUDO' then begin
    TVLCT,imageStruct.red,imageStruct.green,imageStruct.blue
    TV,TEMPORARY(imageStruct.image),xOffset,yOffset
  endif else begin ;(*pState).colorMode EQ 'TRUE':
    if imageStruct.imageColorMode EQ 'PSEUDO' then begin
      DEVICE,DECOMPOSED=0
      TVLCT,imageStruct.red,imageStruct.green,imageStruct.blue
      TV,TEMPORARY(imageStruct.image),xOffset,yOffset
    endif else begin ;imageStruct.imageColorMode EQ 'TRUE':
      DEVICE,DECOMPOSED=1
      TV,TEMPORARY(imageStruct.image),xOffset,yOffset,TRUE=1
    endelse
  endelse
  WIDGET_CONTROL,(*pState).fileText,SET_VALUE=(*(*pState).files)[fileID-1]
endif
END
;*********************************************************************************************


;*********************************************************************************************
PRO IMAGE_VIEWER_CLEANUP,widgetID
;THIS PROCEDURE IS CALLED WHEN THE PROGRAM IS TERMINATED AND XMANAGER REGISTERS A CLEANUP:
;obtain state structure for top-level-base from its uvalue:
WIDGET_CONTROL,widgetID,GET_UVALUE=pState
;test for validity of state structure pointer:
if PTR_VALID(pState) then begin
  ;reset original settings:
  !QUIET=(*pState).quietInit
  !ORDER=(*pState).orderInit
  !P.BACKGROUND=(*pState).backInit
  CD,(*pState).currentDir
  DEVICE,DECOMPOSED=(*pState).dc
  TVLCT,(*pState).r,(*pState).g,(*pState).b
  ;cleanup heap memory:
  PTR_FREE,TEMPORARY((*pState).files)
  numImages=N_ELEMENTS(*(*pState).images)
  if numImages NE 0 then PTR_FREE,*(*pState).images
  PTR_FREE,TEMPORARY((*pState).images)
  PTR_FREE,TEMPORARY(pState)
endif
RETURN
END
;*********************************************************************************************


;*********************************************************************************************
PRO IMAGE_VIEWER_LAUNCH_EVENT,event
;THIS PROCEDURE IS CALLED WHEN A USER RESIZES THE TOP-LEVEL BASE
;error handling:
!ERROR_STATE.CODE=0
CATCH,error
if error NE 0 then begin
  HELP,/LAST_MESSAGE,OUTPUT=traceback
  messageStr=['Error Caught :','',traceback]
  dummy=DIALOG_MESSAGE(messageStr,/ERROR)
  RETURN
endif
;obtain state structure for top-level-base from its UVALUE:
WIDGET_CONTROL,event.top,GET_UVALUE=pState
;reset widget size:
WIDGET_CONTROL,event.top,XSIZE=(*pState).tlbWidth,YSIZE=(*pState).tlbHeight,XOFFSET=0,YOFFSET=0
END
;*********************************************************************************************


;*********************************************************************************************
PRO IMAGE_VIEWER_LAUNCH,event,NO_OPTIONS=no_options
;THIS PROCEDURE IS CALLED WHEN A USER FIRST LAUNCHES THE PROGRAM AFTER SELECTING A SCREEN SIZE
if KEYWORD_SET(no_options) then begin ;800 x 600 mode:
  xSize=800
  ySize=600
  fileXSize = 50
  state = TEMPORARY(event)
endif else begin ;user has selected a mode:
  ;obtain state structure for top-level-base from its uvalue:
  WIDGET_CONTROL,event.top,GET_UVALUE=state,/NO_COPY
  ;obtain UVALUE in order to determine which screen size was selected:
  WIDGET_CONTROL,event.id,GET_UVALUE=uval
  WIDGET_CONTROL,event.top,/DESTROY
  case uval of
     0 : begin ;800 x 600 mode:
           xSize=800
           ySize=600
           fileXSize = 50
         end
     1 : begin ;1024 x 768 mode:
           xSize=1024
           ySize=768
           fileXSize = 75
         end
     2 : begin ;1280 x 1024 mode:
           xSize=1280
           ySize=1024
           fileXSize = 100
         end
     3 : begin ;1600 x 1200 mode:
           xSize=1600
           ySize=1200
           fileXSize = 125
         end
     4 : begin ;larger mode:
           xSize=state.screenSize[0]
           ySize=state.screenSize[1]
           fileXSize = 125
         end
  endcase
endelse
if STRUPCASE(!VERSION.OS_FAMILY) EQ 'UNIX' then begin
  drawXSize = xSize - 330
  drawYSize = ySize - 120
  thumbYSize = ySize - 150
endif else begin
  drawXSize = xSize - 315
  drawYSize = ySize - 120
  thumbYSize = ySize - 118
endelse
;create GUI:
tlb=WIDGET_BASE(TITLE='Image Viewer',/ROW,MBAR=menuBar,/TLB_SIZE_EVENTS,XOFFSET=0,YOFFSET=0)
  fileMenu=WIDGET_BUTTON(menuBar,VALUE='File',/MENU)
    fileBttn1=WIDGET_BUTTON(fileMenu,VALUE='Open Picture Files',EVENT_PRO='image_viewer_open_files')
    fileBttn2=WIDGET_BUTTON(fileMenu,VALUE='Open All In Folder',EVENT_PRO='image_viewer_open_folder')
    fileBttn3=WIDGET_BUTTON(fileMenu,VALUE='Exit',EVENT_PRO='image_viewer_exit')
  helpMenu=WIDGET_BUTTON(menuBar,VALUE='Help',/MENU)
    helpBttn1=WIDGET_BUTTON(helpMenu,VALUE='About IMAGE_VIEWER',EVENT_PRO='image_viewer_help')
  controlsBase=WIDGET_BASE(tlb,/COLUMN,/FRAME,/ALIGN_TOP)
    labelBase=WIDGET_BASE(controlsBase,/COLUMN,SCR_XSIZE=280)
      thumbLabel=WIDGET_LABEL(labelBase,/ALIGN_CENTER,VALUE='Click On Thumbnail To View Image')
    thumbBase=WIDGET_BASE(controlsBase,/COLUMN,/ALIGN_TOP,/FRAME,XSIZE=260,YSIZE=thumbYSize+50,$
                          /SCROLL,X_SCROLL_SIZE=260,Y_SCROLL_SIZE=thumbYSize)
  imageBase=WIDGET_BASE(tlb,/COLUMN,/FRAME,/ALIGN_TOP)
    fileBase=WIDGET_BASE(imageBase,/ROW,/ALIGN_CENTER)
      fileLabel=WIDGET_LABEL(fileBase,VALUE='Current Image File = ')
      fileText=WIDGET_TEXT(fileBase,XSIZE=fileXSize,YSIZE=1)
    imageDraw=WIDGET_DRAW(imageBase,XSIZE=drawXSize,YSIZE=drawYSize,RETAIN=2)
;display the GUI on the computer monitor:
WIDGET_CONTROL,tlb,/REALIZE
;obtain the top-level base geometry:
tlbGeom=WIDGET_INFO(tlb,/GEOMETRY)
tlbWidth=tlbGeom.xsize
tlbHeight=tlbGeom.ysize
if tlbWidth EQ 0 or tlbHeight EQ 0 then begin
  WIDGET_CONTROL,tlb,TLB_GET_SIZE=tlbSize
  tlbWidth=tlbSize[0]
  tlbHeight=tlbSize[1]
endif
;create state structure to store information needed by the other event handling procedures:
pState=PTR_NEW({files:PTR_NEW(/ALLOCATE_HEAP),$
                images:PTR_NEW(/ALLOCATE_HEAP),$
                screenSize:state.screenSize,quietInit:state.quietInit,orderInit:state.orderInit,tlb:tlb,$
                statusBase:0L,controlsBase:controlsBase,thumbBase:thumbBase,fileText:fileText,timer:0B,$
                nFiles:0L,currentDir:state.currentDir,imageDraw:imageDraw,dc:state.dc,r:state.r,g:state.g,$
                b:state.b,gifFlag:state.gifFlag,statusSlider:0L,backInit:state.backInit,$
                colorMode:state.colorMode,tlbWidth:tlbWidth,tlbHeight:tlbHeight,currFile:0L,$
                rowBase:0L,increment:0.0,drawXSize:drawXSize,drawYSize:drawYSize,thumbYSize:thumbYSize})
;store this state structure in the uvalue of the top-level-base
;so it can be obtained by other program units:
WIDGET_CONTROL,tlb,SET_UVALUE=pState
;register the GUI with the XMANAGER event handler routine:
XMANAGER,'image_viewer_launch',tlb,CLEANUP='image_viewer_cleanup'
END
;*********************************************************************************************


;*********************************************************************************************
PRO IMAGE_VIEWER
;error handling:
!ERROR_STATE.CODE=0
CATCH,error
if error NE 0 then begin
  HELP,/LAST_MESSAGE,OUTPUT=traceback
  messageStr=['Error Caught :','',traceback]
  dummy=DIALOG_MESSAGE(messageStr,/ERROR)
  !QUIET=quietInit
  !ORDER=orderInit
  !P.BACKGROUND=backInit
  CD,currentDir
  RETURN
endif
;ignore beta and development build versions of IDL because string to float conversion will fail:
betaTest=STRPOS(STRLOWCASE(!VERSION.RELEASE),'beta')
buildTest=STRPOS(STRLOWCASE(!VERSION.RELEASE),'build')
;check to make sure the version of IDL running is 5.5 or newer:
if betaTest EQ -1 and buildTest EQ -1 then begin
  if FLOAT(!VERSION.RELEASE) LT 5.5 then begin
    dummy=dialog_message('IMAGE_VIEWER is only supported in IDL version 5.5 or newer.',/ERROR)
    RETURN
  endif
endif
;check to make sure there is adequate real estate:
DEVICE,GET_SCREEN_SIZE=screenSize,GET_DECOMPOSED=dc,GET_VISUAL_DEPTH=depth
screenArea = (LONG(screenSize[0])*screenSize[1])
if screenArea LT 480000 then begin
  messageStr=['IMAGE_VIEWER requires the computer monitor (Display) to be',$
              'configured in (800 x 600) resolution mode or better.  To change',$
              'the Screen area for your computer monitor simply right-click',$
              'anywhere on the desktop, select Properties from the context',$
              'menu, click on the Settings tab, drag the Screen area slider',$
              'up to an adequate resolution, and Apply the changes.']
  dummy=DIALOG_MESSAGE(messageStr)
  RETURN
endif
;check in auxiliary license:
result=LMGR("idl_tifflzw",VERSION='1.0')
result=LMGR("idl_gif",VERSION='1.0')
gifFlag=1B
if result NE 1 then begin
  messageStr=['The ability to read GIF (and TIFF LZW compressed) images requires',$
              'an auxiliary license in order to conform with the patent rights of the',$
              'Unisys Corporation.  IMAGE_VIEWER was unable to find the required',$
              'license in this installation.  Consequently, the ability to read GIF files',$
              'will be disabled.']
  dummy=DIALOG_MESSAGE(messageStr)
  gifFlag=0B
endif
;warn users of color flashing if monitor in PseudoColor mode:
if depth LE 8 then begin
  messageStr=['The computer monitor (Display) is currently configured in 8-bit (256 Colors)',$
              'PseudoColor mode.  Due to the dynamic (read+write) nature of the colormap',$
              'system for this visual, when a colortable is loaded for an image it affects',$
              'all visible graphics windows, including the thumbnails of other images.  This',$
              'can lead to a phenomenon known as "color flashing".','',$
              'If possible, it is recommended that you exit this program, reconfigure your',$
              'monitor in 24-bit (TrueColor) mode or better, and restart IMAGE_VIEWER.']
  dummy=DIALOG_MESSAGE(messageStr)
endif
;obtain the current working directory:
CD,CURRENT=currentDir
;attempt to change current working directory to "My Pictures" folder on Windows:
if STRUPCASE(!VERSION.OS_FAMILY) EQ 'WINDOWS' then begin
  ;construct MS-DOS command prompt statement:
  executeStr='cd "%USERPROFILE%\My Documents\My Pictures" & cd'
  SPAWN,executeStr,pathInit,/HIDE
  pathInit=pathInit[0]
  if pathInit EQ '' or STRUPCASE(pathInit) EQ STRUPCASE(!DIR) then begin
    ;temporarily change the current working directory to the C: drive:
    CD,'C:\'
    ;try again:
    SPAWN,executeStr,pathInit,/HIDE
    pathInit=pathInit[0]
  endif
  result=FILE_TEST(pathInit,/READ)
  if result EQ 1 then begin
    CD,pathInit
  endif else begin
    result=FILE_TEST('C:\My Documents\My Pictures',/READ)
    if result EQ 1 then begin
      CD,'C:\My Documents\My Pictures'
    endif else begin
      result=FILE_TEST('C:\',/READ)
      if result EQ 1 then CD,'C:\'
    endelse
  endelse
endif
;suppress informational messaging:
quietInit=!QUIET
!QUIET=1
;store current color mode:
if depth GT 8 then colorMode='TRUE' else colorMode='PSEUDO'
;obtain the current color table:
TVLCT,r,g,b,/GET
LOADCT,0,/SILENT
;force !ORDER=0:
orderInit=!ORDER
!ORDER=0
;force !P.BACKGROUND=0:
backInit=!P.BACKGROUND
!P.BACKGROUND=0
;determine available screen area:
if screenArea GE 786432 and screenArea LT 1310720 then begin
  sizes=STRARR(2)
  sizes[0]='800 x 600'
  sizes[1]='1024 x 768'
endif
if screenArea GE 1310720 and screenArea LT 1920000 then begin
  sizes=STRARR(3)
  sizes[0]='800 x 600'
  sizes[1]='1024 x 768'
  sizes[2]='1280 x 1024'
endif
if screenArea EQ 1920000 then begin
  sizes=STRARR(4)
  sizes[0]='800 x 600'
  sizes[1]='1024 x 768'
  sizes[2]='1280 x 1024'
  sizes[3]='1600 x 1200'
endif
if screenArea GT 1920000 then begin
  sizes=STRARR(5)
  sizes[0]='800 x 600'
  sizes[1]='1024 x 768'
  sizes[2]='1280 x 1024'
  sizes[3]='1600 x 1200'
  sizes[4]=STRTRIM(screenSize[0],2)+' x '+STRTRIM(screenSize[1],2)
endif
;create state structure to store information:
state={screenSize:screenSize,quietInit:quietInit,orderInit:orderInit,currentDir:currentDir,$
       dc:dc,r:r,g:g,b:b,gifFlag:gifFlag,backInit:backInit,colorMode:colorMode}
if screenArea LT 786432 then begin
  IMAGE_VIEWER_LAUNCH,state,/NO_OPTIONS
endif else begin
  ;prompt user to select screen size for program:
  xCenter=screenSize[0]/2
  yCenter=screenSize[1]/2
  tlb=WIDGET_BASE(TITLE='Image Viewer',/COLUMN,/ALIGN_CENTER,TLB_FRAME_ATTR=19)
    spacer=WIDGET_LABEL(tlb,VALUE=' ')
    label=WIDGET_LABEL(tlb,VALUE='     SELECT DESIRED SCREEN SIZE FOR PROGRAM     ')
    spacer=WIDGET_LABEL(tlb,VALUE=' ')
    buttonBase=WIDGET_BASE(tlb,/EXCLUSIVE,/ROW,/ALIGN_CENTER,/FRAME)
      for i=0,N_ELEMENTS(sizes)-1 do begin
        sizeButton=WIDGET_BUTTON(buttonBase,VALUE=sizes[i],UVALUE=i,EVENT_PRO='image_viewer_launch')
      endfor
    spacer=WIDGET_LABEL(tlb,VALUE=' ')
    note=WIDGET_LABEL(tlb,VALUE='Note :  The available screen sizes are dictated by the current screen area')
    note=WIDGET_LABEL(tlb,VALUE='configuration for the computer monitor.  A larger screen size will display images')
    note=WIDGET_LABEL(tlb,VALUE='with better resolution but will decrease the speed at which the program executes.')
    spacer=WIDGET_LABEL(tlb,VALUE=' ')
  geom=WIDGET_INFO(tlb,/GEOMETRY)
  xHalfSize=geom.Scr_XSize/2
  yHalfSize=geom.Scr_YSize/2
  WIDGET_CONTROL,tlb,XOFFSET=xCenter-xHalfSize,YOFFSET=yCenter-yHalfSize
  WIDGET_CONTROL,tlb,/REALIZE
  ;store this state structure in the uvalue of the top-level-base:
  WIDGET_CONTROL,tlb,SET_UVALUE=state
  XMANAGER,'image_viewer',tlb
endelse
END
;*********************************************************************************************