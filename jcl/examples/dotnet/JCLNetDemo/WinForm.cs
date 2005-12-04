using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;
using System.Data;

using Jedi.Jcl;
using Jedi.Jcl.Units;

using Borland.Delphi;
using Borland.Delphi.Units;

using Borland.Vcl;
using Borland.Vcl.Units;

namespace JCLNet
{
	/// <summary>
	/// Zusammenfassende Beschreibung für FrmMain.
	/// </summary>
	public class FrmMain : System.Windows.Forms.Form
	{
		/// <summary>
		/// Erforderliche Designer-Variable.
		/// </summary>
		private System.ComponentModel.Container components = null;
		private System.Windows.Forms.TabControl tcDemos;
		private System.Windows.Forms.TabPage tpSysInfo;
		private System.Windows.Forms.Button btnListProcesses;
		private System.Windows.Forms.TextBox tbProcesses;
		private System.Windows.Forms.ListView lvSpecialDirectories;
		private System.Windows.Forms.Button btnSpecialDirectories;
		private System.Windows.Forms.ColumnHeader columnHeader1;
		private System.Windows.Forms.ColumnHeader columnHeader2;
		private System.Windows.Forms.Label lbIpAddress;
		private System.Windows.Forms.Label lbComputerName;
		private System.Windows.Forms.Button btnQuit;

		public FrmMain()
		{
			//
			// Erforderlich für die Unterstützung des Windows-Form-Designer
			//
			InitializeComponent();

			//
			// TODO: Konstruktorcode nach dem Aufruf von InitializeComponent hinzufügen
			//
		}

		/// <summary>
		/// Ressourcen nach der Verwendung bereinigen
		/// </summary>
		protected override void Dispose(bool disposing)
		{
			if (disposing)
			{
				if (components != null)
				{
					components.Dispose();
				}
			}
			base.Dispose(disposing);
		}

		#region Vom Windows Form-Designer erzeugter Code
		/// <summary>
		/// Erforderliche Methode zur Unterstützung des Designers -
		/// ändern Sie die Methode nicht mit dem Quelltext-Editor
		/// </summary>
		private void InitializeComponent()
		{
			this.tcDemos = new System.Windows.Forms.TabControl();
			this.tpSysInfo = new System.Windows.Forms.TabPage();
			this.lbComputerName = new System.Windows.Forms.Label();
			this.lbIpAddress = new System.Windows.Forms.Label();
			this.btnSpecialDirectories = new System.Windows.Forms.Button();
			this.lvSpecialDirectories = new System.Windows.Forms.ListView();
			this.columnHeader1 = new System.Windows.Forms.ColumnHeader();
			this.columnHeader2 = new System.Windows.Forms.ColumnHeader();
			this.tbProcesses = new System.Windows.Forms.TextBox();
			this.btnListProcesses = new System.Windows.Forms.Button();
			this.btnQuit = new System.Windows.Forms.Button();
			this.tcDemos.SuspendLayout();
			this.tpSysInfo.SuspendLayout();
			this.SuspendLayout();
			// 
			// tcDemos
			// 
			this.tcDemos.Controls.Add(this.tpSysInfo);
			this.tcDemos.Location = new System.Drawing.Point(8, 8);
			this.tcDemos.Name = "tcDemos";
			this.tcDemos.SelectedIndex = 0;
			this.tcDemos.Size = new System.Drawing.Size(712, 520);
			this.tcDemos.TabIndex = 2;
			// 
			// tpSysInfo
			// 
			this.tpSysInfo.Controls.Add(this.lbComputerName);
			this.tpSysInfo.Controls.Add(this.lbIpAddress);
			this.tpSysInfo.Controls.Add(this.btnSpecialDirectories);
			this.tpSysInfo.Controls.Add(this.lvSpecialDirectories);
			this.tpSysInfo.Controls.Add(this.tbProcesses);
			this.tpSysInfo.Controls.Add(this.btnListProcesses);
			this.tpSysInfo.Location = new System.Drawing.Point(4, 22);
			this.tpSysInfo.Name = "tpSysInfo";
			this.tpSysInfo.Size = new System.Drawing.Size(704, 494);
			this.tpSysInfo.TabIndex = 0;
			this.tpSysInfo.Text = "Jedi.Jcl.JclSysInfo";
			// 
			// lbComputerName
			// 
			this.lbComputerName.Location = new System.Drawing.Point(16, 384);
			this.lbComputerName.Name = "lbComputerName";
			this.lbComputerName.Size = new System.Drawing.Size(208, 16);
			this.lbComputerName.TabIndex = 7;
			this.lbComputerName.Text = "ComputerName";
			// 
			// lbIpAddress
			// 
			this.lbIpAddress.Location = new System.Drawing.Point(16, 360);
			this.lbIpAddress.Name = "lbIpAddress";
			this.lbIpAddress.Size = new System.Drawing.Size(136, 16);
			this.lbIpAddress.TabIndex = 6;
			this.lbIpAddress.Text = "IPAddress";
			// 
			// btnSpecialDirectories
			// 
			this.btnSpecialDirectories.Location = new System.Drawing.Point(560, 152);
			this.btnSpecialDirectories.Name = "btnSpecialDirectories";
			this.btnSpecialDirectories.Size = new System.Drawing.Size(136, 23);
			this.btnSpecialDirectories.TabIndex = 5;
			this.btnSpecialDirectories.Text = "Show Special Directories";
			this.btnSpecialDirectories.Click += new System.EventHandler(this.btnSpecialDirectories_Click);
			// 
			// lvSpecialDirectories
			// 
			this.lvSpecialDirectories.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
						this.columnHeader1,
						this.columnHeader2});
			this.lvSpecialDirectories.Location = new System.Drawing.Point(8, 16);
			this.lvSpecialDirectories.Name = "lvSpecialDirectories";
			this.lvSpecialDirectories.Size = new System.Drawing.Size(688, 136);
			this.lvSpecialDirectories.TabIndex = 4;
			this.lvSpecialDirectories.View = System.Windows.Forms.View.Details;
			// 
			// columnHeader1
			// 
			this.columnHeader1.Text = "Name";
			this.columnHeader1.Width = 100;
			// 
			// columnHeader2
			// 
			this.columnHeader2.Text = "Directory";
			this.columnHeader2.Width = 450;
			// 
			// tbProcesses
			// 
			this.tbProcesses.Location = new System.Drawing.Point(8, 184);
			this.tbProcesses.Multiline = true;
			this.tbProcesses.Name = "tbProcesses";
			this.tbProcesses.Size = new System.Drawing.Size(688, 168);
			this.tbProcesses.TabIndex = 3;
			this.tbProcesses.Text = "";
			// 
			// btnListProcesses
			// 
			this.btnListProcesses.Location = new System.Drawing.Point(560, 352);
			this.btnListProcesses.Name = "btnListProcesses";
			this.btnListProcesses.Size = new System.Drawing.Size(136, 23);
			this.btnListProcesses.TabIndex = 2;
			this.btnListProcesses.Text = "List Processes";
			this.btnListProcesses.Click += new System.EventHandler(this.btnListProcesses_Click);
			// 
			// btnQuit
			// 
			this.btnQuit.Location = new System.Drawing.Point(640, 536);
			this.btnQuit.Name = "btnQuit";
			this.btnQuit.TabIndex = 3;
			this.btnQuit.Text = "&Quit";
			this.btnQuit.Click += new System.EventHandler(this.btnQuit_Click);
			// 
			// FrmMain
			// 
			this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
			this.ClientSize = new System.Drawing.Size(728, 566);
			this.Controls.Add(this.btnQuit);
			this.Controls.Add(this.tcDemos);
			this.Name = "FrmMain";
			this.Text = "JCL.NET Demo application";
			this.Load += new System.EventHandler(this.FrmMain_Load);
			this.tcDemos.ResumeLayout(false);
			this.tpSysInfo.ResumeLayout(false);
			this.ResumeLayout(false);
		}
		#endregion

		/// <summary>
		/// Der Haupteintrittspunkt für die Anwendung.
		/// </summary>
		[STAThread]
		static void Main()
		{
			Application.Run(new FrmMain());
		}

		private void btnListProcesses_Click(object sender, System.EventArgs e)
		{
			TStrings list = new TStringList();
			JclSysInfo.RunningProcessesList(list, true);
			tbProcesses.Lines = JclStrings.ArrayOf(list);
		}

		private void btnSpecialDirectories_Click(object sender, System.EventArgs e)
		{
			lvSpecialDirectories.Items.Clear();
			lvSpecialDirectories.Items.Add(new ListViewItem(new string[] {"AppdataFolder", JclSysInfo.GetAppdataFolder()}));
			lvSpecialDirectories.Items.Add(new ListViewItem(new string[] {"CommonAppdataFolder", JclSysInfo.GetCommonAppdataFolder()}));
			lvSpecialDirectories.Items.Add(new ListViewItem(new string[] {"CommonDesktopdirectoryFolder", JclSysInfo.GetCommonDesktopdirectoryFolder()}));
			lvSpecialDirectories.Items.Add(new ListViewItem(new string[] {"CommonFavoritesFolder", JclSysInfo.GetCommonFavoritesFolder()}));
			lvSpecialDirectories.Items.Add(new ListViewItem(new string[] {"CommonProgramsFolder", JclSysInfo.GetCommonProgramsFolder()}));
			lvSpecialDirectories.Items.Add(new ListViewItem(new string[] {"CookiesFolder", JclSysInfo.GetCookiesFolder()}));
			lvSpecialDirectories.Items.Add(new ListViewItem(new string[] {"CurrentFolder", JclSysInfo.GetCurrentFolder()}));
			lvSpecialDirectories.Items.Add(new ListViewItem(new string[] {"DesktopDirectoryFolder", JclSysInfo.GetDesktopDirectoryFolder()}));
			lvSpecialDirectories.Items.Add(new ListViewItem(new string[] {"DesktopFolder", JclSysInfo.GetDesktopFolder()}));
			lvSpecialDirectories.Items.Add(new ListViewItem(new string[] {"FavoritesFolder", JclSysInfo.GetFavoritesFolder()}));
			lvSpecialDirectories.Items.Add(new ListViewItem(new string[] {"HistoryFolder", JclSysInfo.GetHistoryFolder()}));
			lvSpecialDirectories.Items.Add(new ListViewItem(new string[] {"InternetCacheFolder", JclSysInfo.GetInternetCacheFolder()}));
			lvSpecialDirectories.Items.Add(new ListViewItem(new string[] {"PersonalFolder", JclSysInfo.GetPersonalFolder()}));
			lvSpecialDirectories.Items.Add(new ListViewItem(new string[] {"ProgramFilesFolder", JclSysInfo.GetProgramFilesFolder()}));
			lvSpecialDirectories.Items.Add(new ListViewItem(new string[] {"ProgramsFolder", JclSysInfo.GetProgramsFolder()}));
			lvSpecialDirectories.Items.Add(new ListViewItem(new string[] {"RecentFolder", JclSysInfo.GetRecentFolder()}));
			lvSpecialDirectories.Items.Add(new ListViewItem(new string[] {"SendToFolder", JclSysInfo.GetSendToFolder()}));
			lvSpecialDirectories.Items.Add(new ListViewItem(new string[] {"StartmenuFolder", JclSysInfo.GetStartmenuFolder()}));
			lvSpecialDirectories.Items.Add(new ListViewItem(new string[] {"StartupFolder", JclSysInfo.GetStartupFolder()}));
			lvSpecialDirectories.Items.Add(new ListViewItem(new string[] {"TemplatesFolder", JclSysInfo.GetTemplatesFolder()}));
			lvSpecialDirectories.Items.Add(new ListViewItem(new string[] {"WindowsSystemFolder", JclSysInfo.GetWindowsSystemFolder()}));
			lvSpecialDirectories.Items.Add(new ListViewItem(new string[] {"WindowsTempFolder", JclSysInfo.GetWindowsTempFolder()}));
		}
		
		private void FrmMain_Load(object sender, System.EventArgs e)
		{
			lbIpAddress.Text = "IP: " + JclSysInfo.GetIPAddress(JclSysInfo.GetLocalComputerName());
			lbComputerName.Text = "Machine Name: " + JclSysInfo.GetLocalComputerName();
		}
		
		private void btnQuit_Click(object sender, System.EventArgs e)
		{
			Application.Exit();
		}

	}
}
