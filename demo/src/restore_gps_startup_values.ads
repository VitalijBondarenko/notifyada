--  Code borrowed from this page:
--  https://github.com/AdaCore/gnatstudio/issues/31

procedure Restore_GPS_Startup_Values;
--  With GtkAda programs, setting environment variables such as
--  XDG_DATA_DIRS may puzzle execution from GNAT Studio (formerly GPS),
--  for instance issuing errors as : "Couldn't recognize the image file
--  format for file".
--  GNAT Studio sets many of them in execution script, so execution of GtkAda
--  program from GNAT Studio may produce these kind of errors.
--  The workaround is to execute the program from Terminal, same for
--  debugging with GDB.
--  But before executing GNAT Studio executable, the gnatstudio script saves
--  GTK env var in GPS_STARTUP env var.
--  So came up the idea to restore the original values before doing
--  Gtk.Main.Init.
