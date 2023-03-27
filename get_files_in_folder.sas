/*From Lex Jansen*/

%macro get_files_in_folder(dataset_name, folder);

data &dataset_name.;
	keep filename;
	length fref $8 filename $80;
	rc = filename(fref, &folder.);
	if rc = 0 then do;
		did = dopen(fref);
		rc = filename(fref);
	end;
	else do;
		length msg $200.;
		msg = sysmsg();
		put msg=;
		did = .;
	end;
	if did <= 0 then putlog 'ERR' 'OR: Unable to open directory.';

	dnum = dnum(did);
	do i = 1 to dnum;
		filename = dread(did, i);
		/* If this entry is a file, then output. */
		fid = mopen(did, filename);
		if fid > 0 then output;
	end;
	rc = dclose(did);
run;

%mend;
