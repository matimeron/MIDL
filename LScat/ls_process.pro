Pro LS_process, file, inp_dir= idr, out_dir= odr, cut= cut, net= net, last=lst,$
	check= chk, verbose= vrb, show= sho, wait= wai, save_img= sim, _extra= _e

;+
; NAME:
;		LS_PROCESS
; VERSION:
;		8.36
; PURPOSE:
;		Processes multiple LS files, saving the outputs.
; CATEGORY:
;		LS calculations.
; CALLING SEQUENCE:
;		LS_PROCESS, FILE [,keywords]
; INPUTS:
;	FILE
;		Full or partial file name (file only, no path info) may include wild
;		cards.  All files in the input directory (see below) fitting FILE will
;		be processed.  If not given, FILE defaults to '*'.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
; 	INP_DIR
; 		Complete file name for the input folder.  If not given, is querried
; 		about interactively.
; 	OUT_DIR
; 		Same as INP_DIR for the output folder.  The folder may be created
; 		during the call.
; 	CUT
; 		Scalar or 2-element vector specifying the fraction(s) of the data to be
; 		cut away prior to fitting.  If given as a scalar, the second value is
; 		taken to be 1.  Must have 0 <= CUT[0] < CUT[1] <=1.
;	/NET
;		Switch.  If set and if /SHOW is set, the data and image (if /SAVE_IMG
;		is set) are saved with the fitted background subtracted.
; 	/LAST
; 		Switch.  If set, the INP_DIR, OUT_DIR and CUT values from the previous
; 		call are recycled (if there was no previous call, an error will result).
;
; 		Note:	Any of INP_DIR, OUT_DIR and CUT may still be provided when
; 				LAST is set, and the provided value will override the previous
; 				one.
; 		Note2:	The keyword /INTERACTIVE (see function FILE_GET) may be used,
; 				with LAST, to modify the folder selections.
; 	/CHECK
; 		Switch.  When set, the cut values can be checked and adjusted
; 		interactively, scan by scan.
;	/VERBOSE
;		Switch.  If set, the names of the processed files are displayed to the
;		screen.
;	/SHOW
;		Switch.  If set, the processed data for each scan, together with the
;		fit, is displayed to the screen.
;	/WAIT
;		Switch.  If set and SHOW is set, the program pauses after each plot,
;		waiting for the user to hit a key to continue.  Alternatively, if WAIT 
;		is given as negative value, the program waits for the time specified by 
;		the absolute value of WAIT before displaying the next plot.
;	/SAVE_IMG
;		Switch.  If set, SHOW is set automatically and the plots generated are
;		saved as JPG.
;	_EXTRA
;		A formal keyword used to pass keywords to imbedded routines.  Not to be
;		used directly.
; OUTPUTS:
;		Optional screen output and saved files.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		LS_RINFO.  Contains:
;			LEXS	:	Status variable, set to 1 when previous data is defined
;			LIDR	:	Full name of last input folder.
;			LODR	:	Full name of last output folder.
;			LCUT	:	Last CUT values.
; SIDE EFFECTS:
;		If the output folder already contains files with same names as the new
;		ones being written, the older files will be overwritten with no warning.
; RESTRICTIONS:
;		The input folder must exist and contain at least one file of the type
;		specified by the FILE input.
; PROCEDURE:
;		LS_PROCESS queries interactively (if needed) for input and output
;		directory names and processes all files from the input directory fitting
;		the template FILE.  Each of the files is read and fit to a Lorentzian,
;		plus quadratic background.  Then the background is subtracted from the
;		input data and the resulting data is saved to the output folder as a
;		text file.  The save name for each file is the original name + _BGS.  If
;		the keyword SAVE_IMG is set, the plot of the background subtracted data
;		and the fit is also saved for each input file, as JPG.  The name of
;		such saved file is same as this for the data (but with .JPG extension).
;		Finally, a "master file", containing the file names and fit parameters
;		for all the files processed is saved in the output folder.
;
;		LS_PROCESS calls LS_FIT.  Also calls CLEAN_NAME, DEFAULT, FILE_GET, 
;		FNAMPARSE, ISNUM, LEGEND_MM, POLEVAL, TABULATE, WASCII and WIMG_MM, 
;		from MIDL.
; MODIFICATION HISTORY:
;		Created 25-NOV-2011 by Mati Meron.
;		Modified 5-NOV-2013 by Mati Meron.  Internal changes, streamlined
;		operation by transferring number crunching and display to LS_FIT.
;		Modified 10-JAN-2015 by Mati Meron.  Added keyword NET.  Some internal 
;		changes.
;-

	common LS_rinfo, lexs, lidr, lodr, lcut
	on_error, 1

	ext = '.txt'
	pext = '.jpg'
	if keyword_set(net) then qua = '_bgs' else qua = ''
	rtn = string(13b)

	if keyword_set(lst) then begin
		if Default(lexs,0) then begin
			idir = File_get($
			Default(idr,lidr),/dir,title='Select input folder',_extra=_e)
			odir = File_get($
			Default(odr,lodr),/dir,title='Select output folder',_extra=_e)
			cut = ([Default(cut,lcut),0])[0:1] > 0
		endif else message, '"LAST" not defined yet!
	endif else begin
		idir = File_get(idr,/dir,title='Select input folder')
		odir = File_get(odr,/dir,title='Select output folder')
		cut = ([Default(cut,0),1.])[0:1] > 0
	endelse
	lidr = idir
	lodr = odir
	if cut[0] ge cut[1] then begin
		message, 'unacceptable CUT, ignored', /con
		cut = [0.,1.]
		if not Isnum(lexs) then lcut = cut
	endif else lcut = cut
	lexs = 1

	wfil = Fnamparse(Default(file,'*',/dtyp))
	flist = file_search(idir+wfil+ext,count=nfil)
	if nfil eq 0 then begin
		message, 'No data files found!', /con
		return
	end
	pars = fltarr(6,nfil)
	fnams = strarr(nfil)
	for i = 0, nfil-1 do fnams[i] = Fnamparse(flist[i])
	mnam = strmid(fnams,0,strpos(fnams[0],'_'))
	if (where(mnam ne mnam[0]))[0] lt 0 then begin
		mnam = mnam[0]
		bnam = byte(mnam)
		if bnam[0] ge 97 and bnam[0] le 122 then bnam[0] = bnam[0]-32b
		mnam = string(bnam)
	endif else message, 'Mixed files in input folder, exiting!'

	vrbfl = keyword_set(vrb)
	shofl = keyword_set(sho)
	simfl = keyword_set(sim)
	plfl = shofl or simfl
	wai = Default(wai,0-shofl)

	print
	for i = 0, nfil - 1 do begin
		if vrbfl then print, '	Processing ' + fnams[i], i+1, nfil, rtn, $
		form = '(a,"	",i0," of ",i0,a)'
		par = LS_fit(flist[i],cut=cut,net=net,cdata=cdat,error=err,check=chk,$
			show=plfl,_extra= _e)
		pars[[0,2,4],i] = par[3:*]
		pars[[1,3,5],i] = err[3:*]
		Wascii, cdat, odir + fnams[i] + '_bgs' + ext, /auto, call=2
		if simfl then $
		Wimg_mm, odir+ fnams[i] + qua + pext, /jpg, qua=100, /auto, call=2
		if wai ne 0 and (i lt (nfil-1)) then begin
			if wai gt 0 then begin
				print, string([13b,9b,9b]) + 'Hit any key to continue' + rtn
				dum = (dum = get_kbrd())
			endif else wait, abs(wai)
		endif
	endfor
	print, '				DONE!'

	tit = Clean_name(Default(tit,mnam + ' Data'),/wild,/keep_space)
	Tabulate,fnams,pars[0,*],pars[1,*],pars[2,*],pars[3,*],pars[4,*],pars[5,*],$
	header = ['File','Amp','Amp_err','Cent','Cent_err','Hwid','Hwid_err'], $
	form=['a24','6f9.4'],title=tit, file= odir+tit+ext,wid=96,/ind,fir=1,/auto,$
	call=2

	return
end