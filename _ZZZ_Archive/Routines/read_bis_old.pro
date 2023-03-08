Function Read_BIS_old, filename, rot, initialize = ini, nodata = nod, full = ful, $
	itopsize = top, keep_off = kof, show = sho, tables = tab, $
	dimensions = dim, pixsize = pix, _extra = _e

;+
; NAME:
;		READ_BIS
; VERSION:
;		8.41
; PURPOSE:
;		Reads BIS format CCD data.
; CATEGORY:
;		Data Input/Output.
; CALLING SEQUENCE:
;		Result = READ_BIS( FILENAME [, keywords])
; INPUTS:
;	FILENAME
;		CCD data filename.  If not given will be querried for interactively.
; OPTIONAL INPUT PARAMETERS:
;	ROT
;		Integer scalar, specifies rotation as in the IDL ROTATE function. 
; KEYWORD PARAMETERS:
;	/INITIALIZE
;		Switch.  If set, the common block (see below) is reinitialized.  Only
;		needed if the data structure definitions are changed.
;	/NODATA
;		Switch.  If set, only the DIMENSIONS and PIXSIZE info is read, and the
;		image data is ignored.  The function returns 0 in this case.
;	/FULL
;		Switch.  If set, the output is a structure containing, in addition to
;		the CCD image, selected info from the file's header block.  Note that 
;		FULL is ignored when NODATA is set.
;	ITOPSIZE
;		Integer scalar, when given the maximal output image size is 
;		ITOPSIZE x ITOPSIZE.  Images of larger size are rebinned down as needed.
;		Smaller sizes remain unchanged.  The value provided to ITOPSIZE is 
;		rounded internally to the nearest power of 2.
;	/KEEP_OFF
;		Switch.  If set, the data offset is kept.  By default it is subtracted
;		from the data.
;	/SHOW
;		Switch.  If set, an image of the data read is displayed.  SHOW is 
;		ignored when NODATA is set.
;	/TABLES
;		Switch.  If set, the minima and maxima of data in the overflow tables
;		are printed to the screen, for diagnostic purposes.
;	DIMENSIONS
;		Optional output, see below.
;	PIXSIZE
;		Optional output, see below.
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to embedded
;		routines.  Not to be used directly.
; OUTPUTS:
; 		Basic output is a 2D unsigned long array, containing the CCD image.
; 		If  the keyword /FULL is set, the output is a structure containing a 
; 		pointer to the data array, and a HEAD structure containing select info
; 		from the file header (see BIS_HEAD__DEFINE for details).
;
;		Note:	The function returns 0 when NODATA is set.
; OPTIONAL OUTPUT PARAMETERS:
;	DIMENSIONS
;		Returns the dimensions of the data as a 2_element long vector.
;	PIXSIZE
;		Returns the pixel size of the image, in mm.
; COMMON BLOCKS:
;		BIS_KEEP.  Contains
;			EXS	-	A flag, 1 for initialized block, 0 for non-initialized.
;			EXIND -	Indices of "exceptional" header elements.
;			SHIND -	Indices needed for short  readout.	
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None other that the data file must exist.
; PROCEDURE:
;		Straightforward, relies on the BIS data definitions (see there).
;		Calls DEFAULT, DISPLAY_MM, FILE_GET, FLTROUND, ISNUM, STREQ, 
;		STRMATCH_MM and STRPARSE_MM, from MIDL.
; MODIFICATION HISTORY:
;		Created 25-JUL-2009 by Mati Meron.
;		Modified 20-SEP-2009 by Mati Meron.  Internal changes.
;		Modified 25-MAR-2010 by Mati Meron.  Internal changes.
;		Modified 25-JAN-2011 by Mati Meron.  Introduced automatic data offset
;		subtraction.  Added keyword KEEP_OFF.
;		Modified 5-FEB-2011 by Mati Meron.  Internal changes.
;		Modified 10-NOV-2014 by Mati Meron.  Internal changes forced by Bruker
;		format change.
;		Modified 15-APR-2015 by Mati Meron.  Internal change.
;-

	common bis_keep, exs, exind, shind

	on_error, 1
	fcheck = 'hdrblks'
	scheck = 'dettype'
	ocheck = 'nexp'
	cr = string([13b,10b])

	if keyword_set(ini) then exs = 0 else exs = Default(exs,0,/dtyp)
	head = {BIS_head}
	tags = tag_names(head)
	if keyword_set(nod) then begin
		datfl = 0
		fulfl = 0
	endif else begin
		datfl = 1
		fulfl= keyword_set(ful)
	endelse

	if not exs then begin
		exlist = ['type', 'title', 'created']
		n = n_elements(exlist)
		exind = lonarr(n)
		for i = 0, n-1 do exind[i] = Strmatch_mm(exlist[i],tags)
		shlist = ['noverfl', 'npixelb', 'nrows', 'ncols']
		n = n_elements(shlist)
		shind = lonarr(n)
		for i = 0, n-1 do shind[i] = Strmatch_mm(shlist[i],tags)
		exs = 1
	endif

	if Isnum(filename) then begin
		rot = filename
		filename = ''
	endif

	wfnam = File_get(filename,stat=stat,_extra=_e)
	if stat then begin
		openr, apun, wfnam, /get_lun
		blklen = 512l
		linlen = 80l
		hlen = 3
		hhead = strarr(hlen)
		rform = strcompress('('+string(hlen)+'a'+string(linlen)+')',/rem)
		readf, apun, hhead, form = rform
		loc = Strmatch_mm(fcheck,hhead,strlen(fcheck))
		if loc ge 0 then begin
			point_lun, apun, 0
			dum = Strparse_mm(hhead[loc],' :	',lis)
			blknum = fix(lis[dum])
		endif else begin
			free_lun, apun
			message, 'File corrupted, exiting!'
		endelse

		hlen = blknum*blklen/linlen
		hhead = strarr(hlen)
		rform = strcompress('('+string(hlen)+'a'+string(linlen)+')',/rem)
		readf, apun, hhead, form = rform

		thead = strmid(hhead,0,7)
		vhead = strmid(hhead,8)
		tlen = strlen(tags)
		if fulfl then j = lindgen(n_elements(tags)) else j = shind
		for i = 0l, n_elements(j) - 1 do begin
			loc = Strmatch_mm(tags[j[i]],thead,tlen[j[i]])
			if loc ge 0 then begin
				lhead = vhead[loc]
				if fulfl then nexfl = ((where(exind eq j[i]))[0] eq -1) $
				else nexfl = 1
				if not nexfl then begin
					if Streq(tags[j[i]],'title') then lhead = vhead[loc:loc+7]
					tem = strtrim(lhead)
					if Streq(tags[j[i]],'created') then begin
						dum = Strparse_mm(tem,' 	',lis)
						tem = strjoin(lis,' ')
					endif
				endif else dum = Strparse_mm(lhead,' 	',tem)
				if Streq(tags[j[i]],'nrows') or Streq(tags[j[i]],'ncols') $
				then tem = tem[0]
				head.(j[i]) = tem
			endif
		endfor

		dim = [head.ncols,head.nrows]
		loc = Strmatch_mm(scheck,hhead,strlen(scheck))
		if loc ge 0 then begin
			dum = Strparse_mm(hhead[loc],' :	',lis)
			pix = Fltround(10.*512/(float(lis[2])*dim[0]),dig=3)
		endif
		if not keyword_set(kof) then begin
			loc = Strmatch_mm(ocheck,hhead,strlen(ocheck))
			if loc ge 0 then begin
				dum = Strparse_mm(hhead[loc],' :	',lis)
				doff = long(lis[3])
			endif else doff = 0l
		endif else doff = 0l

		if datfl then begin
			tabfl = keyword_set(tab)
			off = blknum*blklen	
			point_lun, apun, off

			case head.npixelb[0] of
				1	:	img =  bytarr(dim)
				2	:	img = uintarr(dim)
				4	:	img = ulonarr(dim)
				else:	message, 'Unrecognizable data format, exiting!'
			endcase
			readu, apun, img
			img = ulong(img)
			off = off + head.npixelb[0]*head.nrows*head.ncols
			point_lun, apun, off
		
			if head.noverfl[0] gt 0 then begin
				case head.npixelb[1] of
					1	:	und = bytarr(head.noverfl[0])
					2	:	und = uintarr(head.noverfl[0])
					else:	message, 'Unrecognizable underflow format, exiting!'
				endcase
				base = 2l^(8*head.npixelb[1]) - 1
				readu, apun, und
				poff = head.noverfl[0]*head.npixelb[1]
				off = off + ceil(1.*poff/16)*16
				point_lun, apun, off
				undfl = 1
			endif else undfl = 0
	
			if head.npixelb[0] eq 1 and head.noverfl[1] gt 0 then begin
				ov1 = uintarr(head.noverfl[1])
				readu, apun, ov1
				if tabfl then print, cr, head.noverfl[1], ' 1-byte overflows;',$
				'range: ', min(ulong(ov1),max=max), max, $
				form= '(a,i8,a,t34,a,i8," - ",i8)'
				poff = head.noverfl[1]*2
				off = off + ceil(1.*poff/16)*16
				point_lun, apun, off
				ov1fl = 1
			endif else ov1fl = 0
	
			if head.npixelb[0] le 2 and head.noverfl[2] gt 0 then begin
				ov2 = ulonarr(head.noverfl[2])
				readu, apun, ov2
				if tabfl then print, head.noverfl[2], ' 2-byte overflows;', $
				'range: ', min(ov2,max=max), max, $
				form= '(i8,a,t32,a,i8," - ",i8)'
				poff = head.noverfl[2]*4
				off = off + ceil(1.*poff/16)*16
				point_lun, apun, off
				ov2fl = 1
			endif else ov2fl = 0
	
			if ov1fl then begin
				dum = where(img eq 255l)
				img[dum] = ov1
			endif
	
			if ov2fl then begin
				dum = where(img eq 65535l)
				img[dum] = ov2
			endif
	
			if undfl then begin
				dum = where(img eq 0)
				img = img + base
				img[dum] = und
			endif

			if doff ge 0 then begin
				img = ulong((long(img) - doff) > 0)
				head.minimum = (head.minimum - doff) > 0
				head.maximum = (head.maximum - doff) > 0
			endif

			wrot = Default(rot,0,/dtyp)
			if wrot ne 0 then img = rotate(img,wrot)
			if keyword_set(sho) then Display_mm, img, /top, /auz, $
			tit = wfnam, xtit = 'pix.', ytit = 'pix.', _extra = _e
		endif else img = 0
		free_lun, apun
	endif else message, 'Bad or missing file!'

	if not fulfl then begin
		res = img
		if Isnum(top) then begin
			if top gt 0 then begin
				wtop = 2l^( 1 > round(alog(top)/alog(2))) < dim[0]
				if wtop lt dim[0] then begin
					wbin = dim/wtop
					res = ulong(rebin(wbin[0]*wbin[1]*double(res),$
					(size(res))[1:2]/wbin))
				endif
			endif
		endif
	endif else res = {BIS_frame,img:ptr_new(img),head:head}

	return, res
end