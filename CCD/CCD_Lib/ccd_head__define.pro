Pro CCD_head__define

;+
; NAME:
;		CCD_HEAD__DEFINE
; VERSION:
;		5.0
; PURPOSE:
;		Defines the {CCD_HEAD} substructure used with CCD (Bruker) data
;		processing routines.
; CATEGORY:
;		Initialization.
; CALLING SEQUENCE:
;		None
; INPUTS:
;		None
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;		None.
; OUTPUTS:
;		None.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Standard.  Defines the structure CCD_HEAD, containing selected data
;		from a SMART type CCD image header, as follows:
;
;			HDRBLKS	:	Header size, in 512 byte blocks, integer.
;			VERSION	:	Header version #, integer.
;			TYPE	:	String indicating type of data in the frame.
;			TITLE	:	String of user comments.
;			NCOUNTS	:	Total number of counts in frame, unsigned long.
;			NOVERFL	:	Number of overflows, unsigned long.
;			MINIMUM	:	Minimum counts in a pixel, unsigned long.
;			MAXIMUM	:	Maximum of counts in a pixel, unsigned long.
;			FILENAM	:	Original frame filename, string.
;			CREATED	:	Date and time of creation, string.
;			NPIXELB	:	Number of bytes per pixel, integer.
;			NROWS	:	Number of data rows, long.
;			NCOLS	:	Number of data colums, long.
; MODIFICATION HISTORY:
;		Created 25-JAN-2005 by Mati Meron.
;-

	on_error, 1

	inflist = [	'hdrblks', 'version',    'type',   'title', $
				'ncounts', 'noverfl', 'minimum', 'maximum', $
				'filenam', 'created', $
				'npixelb',   'nrows', 'ncols'  ]

	dum = create_struct(name = 'CCD_head' ,inflist,$
			0, 0, '', '', 0ul, 0ul, 0ul, 0ul, '', '', 0, 0l, 0l)

	return
end