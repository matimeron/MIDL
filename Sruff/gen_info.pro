Pro Gen_info, file = fname, source = sor, process = prc

    on_error, 1

    datyps = ['Unknown', 'depth', 'energy']
    gst = Gen_struct()

	fname=file_get(fname,filt='dat',path=getenv('sruff_data'),/pick,stat=stat)
	if stat then begin
	    openr, datun, fname, /get_lun, /block
	    readu, datun, gst
	    free_lun, datun
	endif else message, 'Cannot find file!'

    print
    print, 'Processed data from the file ' + strtrim(gst.rawdat,2)
    print, strtrim(string(gst.dsets),2), ' data sets'
    if gst.proc.type gt 0 then $
	print, datyps(gst.proc.type) + '  	= ', gst.dvals(0:gst.dsets-1)
    print
    if keyword_set(sor) then begin
	print, 'Source info:'
	UN_info, unstruct= gst.source
	endif
    if keyword_set(prc) then begin
	print, 'Processing info:'
	PR_info, prstruct = gst.proc
    endif

    return
end
