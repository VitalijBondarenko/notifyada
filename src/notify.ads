------------------------------------------------------------------------------
--                                                                          --
-- Copyright (c) 2013-2015 Vitalij Bondarenko <vibondare@gmail.com>         --
--                                                                          --
------------------------------------------------------------------------------
--                                                                          --
-- The MIT License (MIT)                                                    --
--                                                                          --
-- Permission is hereby granted, free of charge, to any person obtaining a  --
-- copy of this software and associated documentation files (the            --
-- "Software"), to deal in the Software without restriction, including      --
-- without limitation the rights to use, copy, modify, merge, publish,      --
-- distribute, sublicense, and/or sell copies of the Software, and to       --
-- permit persons to whom the Software is furnished to do so, subject to    --
-- the following conditions:                                                --
--                                                                          --
-- The above copyright notice and this permission notice shall be included  --
-- in all copies or substantial portions of the Software.                   --
--                                                                          --
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS  --
-- OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF               --
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.   --
-- IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY     --
-- CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,     --
-- TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE        --
-- SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                   --
------------------------------------------------------------------------------

--  Notification API

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Glib;                  use Glib;
with Gtk.Enums;             use Gtk.Enums;

package Notify is

   function Notify_Init (App_Name : UTF8_String) return Boolean;
   --  Initialized libnotify. This must be called before any other functions.
   --
   --  App_Name : The name of the application initializing libnotify.
   --  Returns  : TRUE if successful, or FALSE on error.

   procedure Notify_Uninit;
   --  Uninitialized libnotify.
   --  This should be called when the program no longer needs libnotify for
   --  the rest of its lifecycle, typically just before exitting.

   function Notify_Is_Initted return Boolean;
   --  Gets whether or not libnotify is initialized.
   --
   --  Returns : TRUE if libnotify is initialized, or FALSE otherwise.

   function Notify_Get_App_Name return UTF8_String;
   --  Gets the application name registered.
   --
   --  Returns : The registered application name, passed to Notify_Init.

   procedure Notify_Set_App_Name (App_Name : UTF8_String);
   --  Sets the application name.
   --
   --  App_Name : The name of the application.

   function Notify_Get_Server_Caps return Gtk.Enums.String_List.Glist;
   --  Synchronously queries the server for its capabilities and returns them
   --  in a String_List.
   --
   --  Returns : a String_List of server capability strings. Free the list
   --            with Free_String_List.

   function Notify_Get_Server_Info
     (Name         : out String_Ptr;
      Vendor       : out String_Ptr;
      Version      : out String_Ptr;
      Spec_Version : out String_Ptr) return Boolean;
   --  Synchronously queries the server for its information, specifically, the
   --  name, vendor, server version, and the version of the notifications
   --  specification that it is compliant with.
   --
   --  Name         : a location to store the server name, or NULL.
   --  Vendor       : a location to store the server vendor, or NULL.
   --  Version      : a location to store the server version, or NULL.
   --  Spec_Version : a location to store the version the service is compliant
   --                 with, or NULL.
   --  Returns      : TRUE if successful, and the variables passed will be set,
   --                 FALSE on error.

end Notify;
