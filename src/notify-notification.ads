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

--  Notify_Notification — A passive pop-up notification.

with Gdk.Pixbuf;   use Gdk.Pixbuf;
with Glib;         use Glib;
with Glib.Object;  use Glib.Object;
with Glib.Error;   use Glib.Error;
with Glib.Properties;
with Glib.Variant; use Glib.Variant;

package Notify.Notification is

   type Notify_Notification_Record is new GObject_Record with null record;
   type Notify_Notification is access all Notify_Notification_Record'Class;

   NOTIFY_EXPIRES_DEFAULT : constant Integer := -1;
   --  The default expiration time on a notification.

   NOTIFY_EXPIRES_NEVER   : constant Integer := 0;
   --  The notification never expires. It stays open until closed by the
   --  calling API or the user.

   type Notify_Urgency is
     (NOTIFY_URGENCY_LOW,
      NOTIFY_URGENCY_NORMAL,
      NOTIFY_URGENCY_CRITICAL);
   pragma Convention (C, Notify_Urgency);
   --  The urgency level of the notification.
   --  NOTIFY_URGENCY_LOW     : Low urgency. Used for unimportant notifications.
   --  NOTIFY_URGENCY_NORMAL  : Normal urgency. Used for most standard
   --                           notifications.
   --  NOTIFY_URGENCY_CRITICAL: Critical urgency. Used for very important
   --                           notifications.

   function Notify_Notification_Get_Type return Glib.GType;

   procedure G_New
     (Notification : out Notify_Notification;
      Summary      : UTF8_String;
      Body_Text    : UTF8_String := "";
      Icon_Name    : UTF8_String := "");
   --  Creates a new Notification. The summary text is required, but all other
   --  parameters are optional.
   --
   --  Notification : Returns the new Notify_Notification.
   --  Summary      : The required summary text.
   --  Body_Text    : The optional body text.
   --  Icon_Name    : The optional icon theme icon name or filename.

   procedure Initialize
     (Notification : not null access Notify_Notification_Record'Class;
      Summary      : UTF8_String;
      Body_Text    : UTF8_String;
      Icon_Name    : UTF8_String);
   --  Initializes a newly created Notification.

   function Update
     (Notification : not null access Notify_Notification_Record;
      Summary      : UTF8_String;
      Body_Text    : UTF8_String := "";
      Icon_Name    : UTF8_String := "") return Boolean;
   --  Updates the notification text and icon. This won't send the update out
   --  and display it on the screen. For that, you will need to call Show.
   --
   --  Notification : The notification to update.
   --  Summary      : The new required summary text.
   --  Body_Text    : The optional body text.
   --  Icon_Name    : The optional icon theme icon name or filename.
   --  Returns      : TRUE, unless an invalid parameter was passed.

   function Show
     (Notification : not null access Notify_Notification_Record;
      Error        : access GError := null) return Boolean;
   --  Tells the notification server to display the notification on the screen.
   --
   --  Notification : The notification.
   --  Error        : The returned error information.
   --  Returns      : TRUE if successful. On error, this will return FALSE and
   --                 set error.

   procedure Set_Timeout
     (Notification : not null access Notify_Notification_Record;
      Timeout      : Integer);
   --  Sets the timeout of the Notification. To set the default time, pass
   --  NOTIFY_EXPIRES_DEFAULT as timeout. To set the notification to never
   --  expire, pass NOTIFY_EXPIRES_NEVER.
   --
   --  Note that the timeout may be ignored by the server.
   --
   --  Notification : The notification.
   --  Timeout      : The timeout in milliseconds.

   procedure Set_Category
     (Notification : not null access Notify_Notification_Record;
      Category     : String);
   --  Sets the category of this Notification. This can be used by the
   --  notification server to filter or display the data in a certain way.
   --
   --  Notification : The notification.
   --  Category     : The category.

   procedure Set_Urgency
     (Notification : not null access Notify_Notification_Record;
      Urgency      : Notify_Urgency);
   --  Sets the urgency level of this notification.
   --  See: Notify_Urgency
   --
   --  Notification : The notification.
   --  Urgency      : The urgency level.

   procedure Set_Image_From_Pixbuf
     (Notification : not null access Notify_Notification_Record;
      Pixbuf       : Gdk_Pixbuf);
   --  Sets the image in the notification from a GdkPixbuf.
   --
   --  Notification : The notification.
   --  Pixbuf       : The image.

   procedure Set_Hint
     (Notification : not null access Notify_Notification_Record;
      Key          : String;
      Value        : GVariant);
   --  Sets a hint for Key with value Value. If Value is NULL, a previously
   --  set hint for key is unset.
   --  If value is floating, it is consumed.
   --
   --  Notification : A Notify_Notification.
   --  Key          : The hint key.
   --  Value        : The hint value, or NULL to unset the hint.

   procedure Set_App_Name
     (Notification : not null access Notify_Notification_Record;
      App_Name     : String);
   --  Sets the application name for the Notification. If this function is not
   --  called or if App_Name is blank, the application name will be set from
   --  the value used in Notify_Init or overridden with Notify_Set_App_Name.
   --
   --  Notification : A Notify_Notification.
   --  App_Name     : The localised application name.

   procedure Clear_Hints
     (Notification : not null access Notify_Notification_Record);
   --  Clears all hints from the notification.
   --
   --  Notification : The notification.

   type Notify_Action_Callback_Void is not null access procedure
     (Notification : Notify_Notification;
      Action       : UTF8_String);
   --  An action callback function.

   procedure Add_Action
     (Notification : not null access Notify_Notification_Record;
      Action       : UTF8_String;
      Label        : UTF8_String;
      Callback     : Notify_Action_Callback_Void);
   --  Adds an Action to a Notification. When the Action is invoked, the
   --  specified Callback function will be called, along with the value passed
   --  to User_Data.
   --
   --  Notification : The notification.
   --  Action       : The action ID.
   --  Label        : The human-readable action label.
   --  Callback     : The action's callback function.

   procedure Clear_Actions
     (Notification : not null access Notify_Notification_Record);
   --  Clears all actions from the notification.
   --
   --  Notification : The notification.

   function Close
     (Notification : not null access Notify_Notification_Record;
      Error        : access GError := null) return Boolean;
   --  Synchronously tells the notification server to hide the notification on
   --  the screen.
   --
   --  Notification : The notification.
   --  Error        : The returned error information.
   --  Returns      : TRUE on success, or FALSE on error with error filled in.

   function Get_Closed_Reason
     (Notification : not null access Notify_Notification_Record) return Integer;
   --  Returns the closed reason code for the Notification. This is valid only
   --  after the "closed" signal is emitted.
   --
   --  Notification : The notification.
   --  Returns      : The closed reason code.

   ----------------------------------
   -- package Add_Action_User_Data --
   ----------------------------------

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Add_Action_User_Data is

      type Notify_Action_Callback_Void is not null access procedure
        (Notification : Notify_Notification;
         Action       : UTF8_String;
         User_Data    : User_Data_Type);
      --  An action callback function.

      procedure Add_Action
        (Notification : not null access Notify_Notification_Record'Class;
         Action       : UTF8_String;
         Label        : UTF8_String;
         Callback     : Notify_Action_Callback_Void;
         User_Data    : User_Data_Type);
      --  Adds an Action to a Notification. When the Action is invoked, the
      --  specified Callback function will be called, along with the value passed
      --  to User_Data.
      --
      --  Notification : The notification.
      --  Action       : The action ID.
      --  Label        : The human-readable action label.
      --  Callback     : The action's callback function.
      --  User_Data    : Custom data to pass to callback.

   end Add_Action_User_Data;

   ----------------
   -- Properties --
   ----------------

   App_Name_Property      : constant Glib.Properties.Property_String;
   --  The application name to use for this notification.
   --  Default value: ""

   Body_Property          : constant Glib.Properties.Property_String;
   --  The message body text.
   --  Default value: ""

   Closed_Reason_Property : constant Glib.Properties.Property_Int;
   --  The reason code for why the notification was closed.
   --  Allowed values: >= -1
   --  Default value : -1

   Icon_Name_Property     : constant Glib.Properties.Property_String;
   --  The icon filename or icon theme-compliant name.
   --  Default value: ""

   Id_Property            : constant Glib.Properties.Property_Int;
   --  The notification ID.
   --  Allowed values: >= 0
   --  Default value : 0

   Summary_Property       : constant Glib.Properties.Property_String;
   --  The summary text.
   --  Default value: ""

   -------------
   -- Signals --
   -------------

   type Cb_Notify_Notification_Void is not null access procedure
     (Self : access Notify_Notification_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Closed : constant Glib.Signal_Name := "closed";
   procedure On_Closed
     (Self  : not null access Notify_Notification_Record;
      Call  : Cb_Notify_Notification_Void;
      After : Boolean := False);
   procedure On_Closed
     (Self  : not null access Notify_Notification_Record;
      Call  : Cb_GObject_Void;
      Slot  : not null access Glib.Object.GObject_Record'Class;
      After : Boolean := False);
   --  The "closed" signal.
   --  Emitted when the notification is closed.

private

   App_Name_Property      : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("app-name");
   Body_Property          : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("body");
   Closed_Reason_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("closed-reason");
   Icon_Name_Property     : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("icon-name");
   Id_Property            : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("id");
   Summary_Property       : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("summary");

end Notify.Notification;
