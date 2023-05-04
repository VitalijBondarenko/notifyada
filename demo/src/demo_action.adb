------------------------------------------------------------------------------
--                                                                          --
-- Copyright (c) 2014-2021 Vitalii Bondarenko <vibondare@gmail.com>         --
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

with Ada.Unchecked_Conversion;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;

with Glib;                     use Glib;
with Glib.Values;              use Glib.Values;
with Gtk.Main;
with Gtk.Enums;                use Gtk.Enums;
                               use Gtk.Enums.String_List;

with Notify;                   use Notify;
with Notify.Notification;      use Notify.Notification;

with Demo_Action_Callbacks;    use Demo_Action_Callbacks;
with Restore_GPS_Startup_Values;

procedure Demo_Action is
   Notification : Notify_Notification;
   R            : Boolean;
   User_Data    : String_Ptr :=
     new UTF8_String'("String passed to the action callback");

   package My_Action is new Add_Action_User_Data (String_Ptr);

begin
   Restore_GPS_Startup_Values;
   Gtk.Main.Init;

   --  Init libnotify.
   R := Notify_Init ("Notify_Ada");

   --  Create new notification.
   G_New
     (Notification => Notification,
      Summary      => "Ada binding to the libnotify.",
      Body_Text    => "Add an action to the notification.",
      Icon_Name    => "");
   Notification.Set_Timeout (NOTIFY_EXPIRES_DEFAULT);

   --  Add action.
   Notification.Add_Action
     (Action       => "default",
      Label        => "Press Me",
      Callback     => Action_Callback'Access);

   --  Add action with user data.
   My_Action.Add_Action
     (Notification => Notification,
      Action       => "user_data_action",
      Label        => "Print Message",
      Callback     => Action_Callback_User_Data'Access,
      User_Data    => User_Data);

   --  Sets the category of the Notification.
   Notification.Set_Category ("presence.online");

   --  Connect signal "closed" handler.
   Notification.On_Closed (On_Closed_Callback'Access);

   --  Show Notification on the screen.
   R := Notification.Show;

   if R then
      Gtk.Main.Main;
   else
      Put_Line ("ERROR: can't display notification.");
   end if;

   Notify_Uninit;
exception
   when others => Notify_Uninit;
end Demo_Action;
