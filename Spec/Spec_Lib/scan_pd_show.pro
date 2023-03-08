Pro Scan_PD_show, snum, fnum, filename= fnam, orientation= ori, display= dsp, $
	raw= raw, angles= ang, tilt= til, title= tit, radius= rad, rcolor= rco, $
	center= cen, relative= rel, xy_reg= xyr, z_reg= zr, xy_int= xyi, z_int= zi,$
	big= big, small= sml,cmark= cmr, yoff=yof, result=res, ppos=pps, range=ran,$
	_extra=_e

;+
; NAME:
;		SCAN_PD_SHOW
; VERSION:
;		8.421
; PURPOSE:
;		Displays a Pilatus Detector file.
; CATEGORY:
;		SPEC Input/Output.
; CALLING SEQUENCE:
;		SCAN_PD_SHOW, SNUM [, FNUM] [, keywords])
; INPUTS:
;	SNUM
;		Single scan number.
;		Note 1:	unlike most other SCAN routines, SCAN_PD_SHOW
;				does not accept multiple scan numbers.
;		Note 2:	SNUM is mandatory when reading from a SPEC file, but not to be
;				used when reading the TIFF file directly.
;
;		Alternatively, SNUM can also be an already read 2D data.  In this case
;		it is displayed as is.  Display region setting and integration are
;		still available but keywords related to data readout are ignored.
;
;		Yet another alternative, SNUM can be an already read and integrated
;		data.  In this case it'll be displayed properly if the integration type
;		is provided, else an error results.
; OPTIONAL INPUT PARAMETERS:
;	FNUM
;		Frame number within the scan.  If not given, defaults to 0.
; KEYWORD PARAMETERS:
;	FILENAME
;		Name of a TIFF file to be read directly (not using SPEC data).
;	ORIENTATION
;		Character input specifying the detector orientation.  Two possible
;		values, "HOR" (for horizontal) and "VER" (for vertical).  First letter
;		is sufficient.  Default is vertical.
;	/DISPLAY
;		Switch.  If set explicitly to 0, no display is generated.  By default
;		it is considered to be 1 (i.e. display is generated).
;	/RAW
;		Switch.  If set, the data is read "raw", meaning the X and Y
;		coordinates are expressed in pixels, not angles or Q-values.
;
;		Note:	When reading directly from a TIFF file, the data is always raw.
;				When reading through a SPEC file, the default coordinates are
;				Q_xy and Q_z.
;	/ANGLES
;		Switch.  If set the coordinates of the image are the angles DTHETA (for
;		horizontal) and BETA for vertical, instead of Q_xy and Q_z.
;
;		Note:	In case of direct read from TIFF file, /ANGLES has no effect.
;	TILT
;		Optional detector tilt angle (couterclockwise) in degrees.  Valid for
;		angle and Q-xy coordinates, has no effect if /RAW is set.
;	TITLE
;		An optional title for the resulting plot/image.  If not given, a
;		default title is generated internally and can be returned through TITLE.
;	RADIUS
;		Numerical scalar, radius of a spherical scatterer, for form-factor 
;		display.
;	RCOLOR
;		Color value to be used when drawing the form-factor scattering 
;		contours.  Default color is white.
;	CENTER
;		2-element vector, specifies the "center" of the frame being displayed
;		(as defined in the SCAN_PD_CENTER routine).  Needed only when the 
;		keywords RELATIVE and/or CMARK (see below) are set.  If given, must be
;		in the units (pixels, degrees ot Q-values) of the data.  If required and
;		not given then:
;			1)	If the data is provided through a scan number, the center values
;				will be read from the SPEC data.
;			2)	If a preread data is input, an error will result.
;	/RELATIVE
;		Switch.  If set, the XY_REG and/or Z_REG limits are taken relative to
;		the CENTER (see above).
;	XY_REG
;		A two element vector specifying horizontal region for integration in
;		[min,max] order.  Optionally, a scalar specifying the lower limit of the
;		region (with the higher limit provided by the data).  If not given, the
;		region is the whole horizontal range present.
;	Z_REG
;		Same as XY_REG, for vertical region.
;
;		Note 1	:	XY_REG and Z_REG can be both specified, to define a
;					rectangular region of interest.
;		Note 2	:	The coordinates (pixel, angle or Q) used in the regions
;					depend on the settings of SCAN_PD_READ.
;	/XY_INT											|	Note:	One and only one
;		Switch.  Specifies integration over Qxy.	|			of these two
;	/Z_INT											|			keywords may
;		Switch.  Specifies integration over Qz. 	|			be set.
;	/BIG
;		Switch.  If set, the display is stretched by a factor of 2	| only one
;		(in each dimension).  Active only for image displays.		| of these
;	/SMALL															| 2 may be
;		Switch.  If set, the display is shrunk by a factor of 2		| set.
;		(in each dimension).  Active only for image displays.		|
;	/CMARK
;		Switch.  If set, the center of the frame is marked with a cross and a
;		vertical line passing through this cross.  Not active when 1D data is
;		displayed.
;
;		Note:	If CMARK=2 is used, the vertical line is omitted.
;	YOFF
;		Specifies an offset (in pixels) from the center location, to be used
;		with CMARK (see above).  If provided, both the original and the offset 
;		centers are marked on the frame (original in white, offset in purple).
;	RESULT
;		Optional output, see below.
;	PPOS
;		Optional output, see below.
;	RANGE
;		Optional output, see below.
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to embedded
;		routines.  This includes (among others) the following keywords,
;		affecting data readout:
;
;			HROI
;				Two element vector defining horizontal region of interest,
;				in *pixels*.
;			VROI
;				Two element vector defining vertical region of interest, in
;				*pixels*.
;			/NORM
;				Switch.  If set, the data is normalized to monitor counts.
;			/BAD
;				Switch.  If set, faulty (very high count rate) pixels are
;				removed from the data.
;			/PINHOLE
;				Switch.  Specifies that pinhole transformations are to be
;				applied to the angles or Q-values.
;
;		See SCAN_PD_READ for more details.
; OUTPUTS:
;		Standard output is graphics only, either a 2D image or a plot of 1D
;		integrated data.  Optional output through the RESULT keyword.
;
;		In the case of 2D display, the contours corresponding to minima of a
;		sperical form-factor can be overlaid on the image.  This is invoked by 
;		providing a radius value through the keyword RADIUS.  The results are
;		only valid when the data is displayed in Q-coordinates. 
; OPTIONAL OUTPUT PARAMETERS:
;	RESULT
;		Returns the data that's being displayed to the screen.  If it is image
;		data, it is returned in the 3D array format of 4 pages: XY-coordinates,
;		Z-coordinates, data, errors.  If it is integrated 1D Data, it is being
;		returned in the standard 3 column format (coordinate, data, errors).  A
;		"just image" data is returned as a 2D image.
;	PPOS
;		Returns a 4-element integer vector containing the "plot position" (see 
;		!P.POSITION) of the image in device units.  
;	RANGE
;		Returns a 4-element floating vector containing the X and Y coo. ranges 
;		of the image in same format as PPOS, i.e. [X_min, Y_min, X_max, Y_max].
;
;		Note:	PPOS and RANGE are provided as outputs by the routine DISPLAY_MM
;				which is used to generate the image.  See there for details.
;		Note2:	PPOS and RANGE are only meaningful for 2D data.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None other that the data files must exist.
; PROCEDURE:
;		Straightforward, uses SCAN_PD_READ to read the data, IMG_INT to
;		process it and either SCAN_SHOW or DISPLAY_MM to display it.  Calls
;		SCAN_PD_CENCOO and SCAN_PD_FTOD.  If needed calls OP_SFF from LD to 
;		display form-factor contours.  Calls DEFAULT, ISNUM, ONE_OF, STRPARSE_MM
;		and WHERINSTRUCT, from MIDL.  Also calls SDEP from imports.
; MODIFICATION HISTORY:
;		Created 15-AUG-2007 by Mati Meron.
;		Modified and documented 30-OCT-2007 by Mati Meron.
;		Modified 10-NOV-2007 by Mati Meron.  Added keywords TITLE and RESULT.
;		Modified 15-NOV_2007 by Mati Meron.  Internal changes.
;		Modified 15-APR-2008 by Mati Meron.  Eliminated keyword /PIX, no longer
;		needed.
;		Modified 25-APR-2008 by Mati Meron.  Internal changes.
;		Modified 5-MAY-2008 by Mati Meron.  Internal changes.
;		Modified 25-MAR-2010 by Mati Meron.  Internal changes for APEX support.
;		Modified 30-MAR-2010 by Mati Meron.  Internal changes.
;		Modified 5-APR-2010 by Mati Meron.  Added keyword DISPLAY.
;		Modified 10-JUN-2011 by Mati Meron.  Added keywords RADIUS and RCOLOR.
;		Modified 25-OCT-2011 by Mati Meron.  Internal changes.
;		Modified 15-NOV-2013 by Mati Meron.  Added keywords CENTER, RELATIVE, 
;		CMARK, PPOS and RANGE.
;		Modified 15-DEC-2013 by Mati Meron.  Added keyword YOFF.
;		Modified 10-SEP-2014 by Mati Meron.  Internal changes.
;		Modified 20-NOV-2014 by Mati Meron.  Added keyword SMALL.
;		Modified 10-JUL-2015 by Mati Meron.  Added keyword BIG.
;-

	on_error, 1

	xytit = [['pix.','Dth','Q!dxy!n'],['pix.','Beta','Q!dz!n']]
	if keyword_set(raw) or n_params() eq 0 then tn = 0 $
	else tn = 2 - keyword_set(ang)
	dum = Wherinstruct('pin',_e)
	if dum ge 0 then pnfl = _e.(dum) else pnfl = 0

	if Isnum(xyr) then wxyr = xyr
	if Isnum(zr) then wzr = zr
	sizs = (size(snum))[0]
	if (keyword_set(rel) or keyword_set(cmr)) then begin
		if n_elements(cen) ne 2 then begin
			if sizs eq 0 then $
			wcen = Scan_PD_cencoo(snum,fnum,raw=raw,ang=ang,pin=pnfl) $
			else message, 'CENTER undefined!'
		endif else wcen = cen
		if Isnum(wcen) then begin
			if keyword_set(rel) then begin
				if Isnum(wxyr) then wxyr = wxyr + wcen[0]
				if Isnum(wzr) then wzr = zr + wcen[1]
			endif
			cmfl = keyword_set(cmr)
		endif else cmfl = 0
	endif else cmfl = 0

	if sizs eq 0 then dat = Scan_PD_read(snum,fnum,file= fnam,orient= ori, $
	raw= raw,angles= ang,tilt= til,title= dtit,_extra = _e) else dat = snum
	siz = size(dat)
	case siz[0] of
		2	:	begin
					if siz[1] eq 3 then begin
						res = dat
						ityp = One_of(xyi,zi) + 1
						if ityp eq 0 then message, $
						'Integral type must be specified with integrated data!'
					endif else begin
						if siz[1] gt 3 then begin
							tn = 0
							res = Img_int(dat,xy_reg=wxyr,z_reg=wzr,$
							xy_int=xyi,z_int=zi,ityp=ityp,_extra=_e)
						endif else message, 'Unacceptable input!'
					endelse
				end
		3	:	begin
					if siz[1] ge 4 then res= Img_int(dat,xy_reg=wxyr,z_reg=wzr,$
					xy_int=xyi,z_int=zi,ityp=ityp,_extra=_e)$
					else message, 'Unacceptable input!'
				end
		else:	message, 'Unacceptable input!'
	endcase

	if Default(dsp,1,/dtyp) then begin
		tit = Default(tit,Default(dtit,''))	
		if ityp eq 0 then begin
			wtit = tit
			case One_of(big,sml) of
				-1	:	auz = 512l
				0	:	auz = 1024l
				1	:	begin
							auz = 256l
							dum = Strparse_mm(tit,sdep(/ds),lis)
							wtit = lis[dum]
						end
			endcase
			sizr = size(res)
			bin = ceil(1.*sizr[sizr[0]-1:sizr[0]]/auz)
			dum = (Wherinstruct('rot',_e))[0]
			if dum ge 0 then _e.(dum) = 0
			Display_mm, res,bin= bin,auz= auz,/aver,ppos=pps,ran=ran,$
			xtit= xytit[tn,0],ytit= xytit[tn,1],tit= wtit,_extra= _e
			if keyword_set(rad) then $
			Op_sff,rad,ppos=pps,ran=ran,col=rco,_extra=_e
			if cmfl then begin
				plot, ran[[0,2]], ran[[1,3]], pos=pps, /dev, xsty=13, ysty=13, $
				/noerase, /nodata
				plots,wcen[0],wcen[1],col=!pcol.white,psym=7,symsize=1.5,thi=2
				if not cmr/2 then plots, [wcen[0],wcen[0]],ran[[1,3]], $
				col=!pcol.white,thi=1,line=1
				if Isnum(yof) and ((Wherinstruct('pin',_e))[0] ge 0) and $
				(sizs eq 0) then begin
					xof = Scan_PD_ftod(snum,fnum,yfot=yof,/round,_extra=_e)
					ocen = $
					Scan_PD_cencoo(snum,fnum,raw=raw,ang=ang,pin=pnfl,off=xof)
					plots,ocen[0],ocen[1],col=!pcol.purple,psym=7,syms=1.5,thi=2
					if not cmr/2 then plots, [ocen[0],ocen[0]],ran[[1,3]], $
					col=!pcol.purple,thi=1,line=1
				endif
			endif
		endif else Scan_show,res,xtit=xytit[tn,2-ityp],tit=tit,/count,_extra=_e
	endif

	return
end