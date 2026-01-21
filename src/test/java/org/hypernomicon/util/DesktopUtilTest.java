/*
 * Copyright 2015-2026 Jason Winning
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.hypernomicon.util;

import static org.junit.jupiter.api.Assertions.*;
import static org.hypernomicon.util.DesktopUtil.*;

import java.net.URI;

import org.hypernomicon.view.tabs.WorkTabCtrlr;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.EnabledOnOs;

//---------------------------------------------------------------------------

/**
 * Unit test class for the {@code DesktopUtil} class, focusing on URL/URI handling.
 *
 * @see org.hypernomicon.util.DesktopUtil
 */
class DesktopUtilTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Asserts that a URL string was not double-encoded when converted to URI.
   * Double-encoding would turn %XX into %25XX.
   */
  private static void assertNotDoubleEncoded(String originalUrl, URI result)
  {
    if (originalUrl.contains("%"))
    {
      String output = result.toString();

      assertFalse(output.contains("%25"), "URL was double-encoded: " + originalUrl + " -> " + output);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // processWebLink tests: comprehensive verification of URL transformation, action routing,
  // URI construction, and all UrlOpenResult fields

  // === INVALID action tests ===

  @Test
  void testProcessWebLink_EmptyString()
  {
    UrlOpenResult result = processWebLink("", true);

    assertEquals(UrlOpenAction.INVALID, result.action());
    assertNull(result.urlString(), "urlString should be null for empty input");
    assertNull(result.uri(), "uri should be null for INVALID");
    assertNull(result.errorMessage(), "No error message for empty input");
  }

  @Test
  void testProcessWebLink_WhitespaceOnly()
  {
    UrlOpenResult result = processWebLink("   \t\n  ", true);

    assertEquals(UrlOpenAction.INVALID, result.action());
    assertNull(result.urlString());
    assertNull(result.uri());
    assertNull(result.errorMessage());
  }

  @Test
  void testProcessWebLink_Ext1WithoutExtPath()
  {
    UrlOpenResult result = processWebLink("ext1://some/path/file.txt", false);

    assertEquals(UrlOpenAction.INVALID, result.action());
    assertNull(result.urlString(), "urlString should be null when ext1 path unavailable");
    assertNull(result.uri());
    assertEquals(WorkTabCtrlr.NO_EXT_PATH_MESSAGE, result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_UnescapedBracketsInPath()
  {
    // Brackets and spaces are now escaped directly by escapeIllegalUriChars,
    // so this works without fallback paths and without double-encoding.
    UrlOpenResult result = processWebLink("onenote://server/path/[12345] File.one#Section", true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertNotNull(result.uri());
    // Brackets escaped to %5B/%5D, spaces to %20 - no double-encoding
    assertTrue(result.uri().getRawPath().contains("%5B12345%5D"), "Brackets should be escaped");
    assertTrue(result.uri().getRawPath().contains("%20"), "Spaces should be escaped");
    assertFalse(result.uri().getRawPath().contains("%255B"), "Should not double-encode");
    assertNull(result.errorMessage());
  }

  // === LAUNCH_FILE action tests (Windows only) ===

  @Test
  @EnabledOnOs(org.junit.jupiter.api.condition.OS.WINDOWS)
  void testProcessWebLink_WindowsLocalPath_SimpleDrive()
  {
    UrlOpenResult result = processWebLink("C:\\Windows\\Explorer.exe", true);

    assertEquals(UrlOpenAction.LAUNCH_FILE, result.action());
    assertEquals("C:\\Windows\\Explorer.exe", result.urlString());
    assertNull(result.uri(), "URI should be null for LAUNCH_FILE");
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs(org.junit.jupiter.api.condition.OS.WINDOWS)
  void testProcessWebLink_WindowsLocalPath_WithSpaces()
  {
    UrlOpenResult result = processWebLink("C:\\Program Files\\Microsoft Office\\Office16\\WINWORD.EXE", true);

    assertEquals(UrlOpenAction.LAUNCH_FILE, result.action());
    assertEquals("C:\\Program Files\\Microsoft Office\\Office16\\WINWORD.EXE", result.urlString());
    assertNull(result.uri());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs(org.junit.jupiter.api.condition.OS.WINDOWS)
  void testProcessWebLink_WindowsLocalPath_DifferentDrive()
  {
    UrlOpenResult result = processWebLink("D:\\Data\\Documents\\report.pdf", true);

    assertEquals(UrlOpenAction.LAUNCH_FILE, result.action());
    assertEquals("D:\\Data\\Documents\\report.pdf", result.urlString());
    assertNull(result.uri());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs(org.junit.jupiter.api.condition.OS.WINDOWS)
  void testProcessWebLink_WindowsLocalPath_WithSpecialChars()
  {
    UrlOpenResult result = processWebLink("C:\\Users\\Test\\Documents\\Report (2024) - Final.docx", true);

    assertEquals(UrlOpenAction.LAUNCH_FILE, result.action());
    assertEquals("C:\\Users\\Test\\Documents\\Report (2024) - Final.docx", result.urlString());
    assertNull(result.uri());
    assertNull(result.errorMessage());
  }

  // === BROWSE_WEB action tests: Plain domains (http:// prepended) ===

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_PlainDomain()
  {
    UrlOpenResult result = processWebLink("example.com", true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertEquals("http://example.com", result.urlString());
    assertNotNull(result.uri());
    assertEquals("http", result.uri().getScheme());
    assertEquals("example.com", result.uri().getHost());
    assertEquals("", result.uri().getPath());  // Empty string, not null, for URI without path
    assertNull(result.uri().getQuery());
    assertNull(result.uri().getFragment());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_PlainDomainWithPath()
  {
    UrlOpenResult result = processWebLink("example.com/path/to/resource", true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertEquals("http://example.com/path/to/resource", result.urlString());
    assertNotNull(result.uri());
    assertEquals("http", result.uri().getScheme());
    assertEquals("example.com", result.uri().getHost());
    assertEquals("/path/to/resource", result.uri().getPath());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_PlainDomainWithQueryAndFragment()
  {
    UrlOpenResult result = processWebLink("example.com/search?q=test#results", true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertEquals("http://example.com/search?q=test#results", result.urlString());
    assertNotNull(result.uri());
    assertEquals("http", result.uri().getScheme());
    assertEquals("example.com", result.uri().getHost());
    assertEquals("/search", result.uri().getPath());
    assertEquals("q=test", result.uri().getQuery());
    assertEquals("results", result.uri().getFragment());
    assertNull(result.errorMessage());
  }

  // === BROWSE_WEB action tests: UNC paths (file:// prepended) ===

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_UncPath_ServerShare()
  {
    UrlOpenResult result = processWebLink("\\\\ServerName\\ShareName\\Folder\\File.txt", true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertEquals("file://ServerName/ShareName/Folder/File.txt", result.urlString());
    assertNotNull(result.uri());
    assertEquals("file", result.uri().getScheme());
    assertEquals("ServerName", result.uri().getHost());
    assertEquals("/ShareName/Folder/File.txt", result.uri().getPath());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_UncPath_IpAddress()
  {
    UrlOpenResult result = processWebLink("\\\\192.168.1.100\\shared\\documents\\report.xlsx", true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertEquals("file://192.168.1.100/shared/documents/report.xlsx", result.urlString());
    assertNotNull(result.uri());
    assertEquals("file", result.uri().getScheme());
    assertEquals("192.168.1.100", result.uri().getHost());
    assertEquals("/shared/documents/report.xlsx", result.uri().getPath());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_UncPath_WithSpaces()
  {
    UrlOpenResult result = processWebLink("\\\\FileServer\\Team Documents\\Project Files\\Report.docx", true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertEquals("file://FileServer/Team Documents/Project Files/Report.docx", result.urlString());
    assertNotNull(result.uri());
    assertEquals("file", result.uri().getScheme());
    assertEquals("FileServer", result.uri().getHost());
    assertNull(result.errorMessage());
  }

  // === BROWSE_WEB action tests: HTTP/HTTPS URLs ===

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_HttpUrl_Simple()
  {
    UrlOpenResult result = processWebLink("http://example.com", true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertEquals("http://example.com", result.urlString());
    assertNotNull(result.uri());
    assertEquals("http", result.uri().getScheme());
    assertEquals("example.com", result.uri().getHost());
    assertEquals(-1, result.uri().getPort());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_HttpUrl_WithPort()
  {
    UrlOpenResult result = processWebLink("http://localhost:8080/api/test", true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertEquals("http://localhost:8080/api/test", result.urlString());
    assertNotNull(result.uri());
    assertEquals("http", result.uri().getScheme());
    assertEquals("localhost", result.uri().getHost());
    assertEquals(8080, result.uri().getPort());
    assertEquals("/api/test", result.uri().getPath());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_HttpsUrl_AllComponents()
  {
    UrlOpenResult result = processWebLink("https://www.example.com:443/path/to/resource?key=value&other=123#section", true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertEquals("https://www.example.com:443/path/to/resource?key=value&other=123#section", result.urlString());
    assertNotNull(result.uri());
    assertEquals("https", result.uri().getScheme());
    assertEquals("www.example.com", result.uri().getHost());
    assertEquals(443, result.uri().getPort());
    assertEquals("/path/to/resource", result.uri().getPath());
    assertEquals("key=value&other=123", result.uri().getQuery());
    assertEquals("section", result.uri().getFragment());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_HttpUrl_WithUserInfo()
  {
    UrlOpenResult result = processWebLink("http://user:pass@example.com/secure", true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertNotNull(result.uri());
    assertEquals("http", result.uri().getScheme());
    assertEquals("user:pass", result.uri().getUserInfo());
    assertEquals("example.com", result.uri().getHost());
    assertEquals("/secure", result.uri().getPath());
    assertNull(result.errorMessage());
  }

  // === BROWSE_WEB action tests: Pre-encoded URLs (verify no double-encoding) ===

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_PreEncodedSpaces_NoDoubleEncoding()
  {
    String url = "https://example.com/path%20with%20spaces/file%20name.txt";
    UrlOpenResult result = processWebLink(url, true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertEquals(url, result.urlString());
    assertNotNull(result.uri());
    assertNotDoubleEncoded(url, result.uri());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_PreEncodedSpacesInFragment_NoDoubleEncoding()
  {
    String url = "http://example.com#section%20with%20spaces";
    UrlOpenResult result = processWebLink(url, true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertNotNull(result.uri());
    assertEquals("http", result.uri().getScheme());
    assertNotNull(result.uri().getFragment());
    assertNotDoubleEncoded(url, result.uri());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_PreEncodedSpecialChars_NoDoubleEncoding()
  {
    String url = "https://example.com/path%2Fwith%2Fencoded%2Fslashes?query=%26ampersand%3Dvalue";
    UrlOpenResult result = processWebLink(url, true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertEquals(url, result.urlString());
    assertNotNull(result.uri());
    assertNotDoubleEncoded(url, result.uri());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_PreEncodedBracketsInPath()
  {
    // OneNote URLs often have pre-encoded brackets %5b/%5d in the path
    String url = "onenote://F/Department/Shared/Repository/Projects/%5b284728%5d%20Project%20Name.one#Designs&section-id=%7B4B15C73B-891D-4C17-B496-E7C3CB1D59EC%7D&page-id=%7BB233466B-8716-40A8-992D-2070521F4174%7D&end";
    UrlOpenResult result = processWebLink(url, true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertNotNull(result.uri());
    assertEquals("onenote", result.uri().getScheme());
    assertNotNull(result.uri().getFragment());
    assertTrue(result.uri().getFragment().contains("section-id"));
    assertTrue(result.uri().getFragment().contains("page-id"));
    assertNotDoubleEncoded(url, result.uri());
    assertNull(result.errorMessage());
  }

  // === BROWSE_WEB action tests: URLs with curly braces (verify escaping) ===

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_CurlyBracesInFragment_ProperlyEscaped()
  {
    String url = "onenote://server/notebook.one#Page&section-id={1A2B3C4D-5E6F-7890-ABCD-EF1234567890}&end";
    UrlOpenResult result = processWebLink(url, true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertEquals(url, result.urlString());
    assertNotNull(result.uri());
    assertEquals("onenote", result.uri().getScheme());
    // Curly braces should be escaped to %7B and %7D
    String uriString = result.uri().toString();
    assertTrue(uriString.contains("%7B") || uriString.contains("%7b"), "Opening brace should be escaped");
    assertTrue(uriString.contains("%7D") || uriString.contains("%7d"), "Closing brace should be escaped");
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_CurlyBracesWithPreEncodedChars_NoDoubleEncoding()
  {
    String url = "onenote://server/path/%5bProject%5d%20Name.one#Section&section-id={ABCD1234-5678-90EF-GHIJ-KLMNOPQRSTUV}&end";
    UrlOpenResult result = processWebLink(url, true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertEquals(url, result.urlString());
    assertNotNull(result.uri());
    assertNotDoubleEncoded(url, result.uri());
    assertNull(result.errorMessage());
  }

  // === BROWSE_WEB action tests: URLs with pipes (verify escaping) ===

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_PipeInQuery_ProperlyEscaped()
  {
    String url = "https://example.com/search?filter=a|b|c";
    UrlOpenResult result = processWebLink(url, true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertEquals(url, result.urlString());
    assertNotNull(result.uri());
    // Pipe should be escaped to %7C
    String uriString = result.uri().toString();
    assertTrue(uriString.contains("%7C") || uriString.contains("%7c"), "Pipe should be escaped");
    assertNull(result.errorMessage());
  }

  // === BROWSE_WEB action tests: Mailto URLs ===

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_Mailto_Simple()
  {
    UrlOpenResult result = processWebLink("mailto:user@example.com", true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertEquals("mailto:user@example.com", result.urlString());
    assertNotNull(result.uri());
    assertEquals("mailto", result.uri().getScheme());
    assertEquals("user@example.com", result.uri().getSchemeSpecificPart());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_Mailto_WithSubject()
  {
    UrlOpenResult result = processWebLink("mailto:user@example.com?subject=Hello%20World", true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertNotNull(result.uri());
    assertEquals("mailto", result.uri().getScheme());
    assertNull(result.errorMessage());
  }

  // === BROWSE_WEB action tests: File URLs ===

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_FileUrl_LocalPath()
  {
    String url = "file:///C:/Windows/System32/notepad.exe";
    UrlOpenResult result = processWebLink(url, true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertEquals(url, result.urlString());
    assertNotNull(result.uri());
    assertEquals("file", result.uri().getScheme());
    assertTrue(result.uri().getPath().contains("notepad.exe"));
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_FileUrl_NetworkPath()
  {
    String url = "file://server.local/share/documents/file.pdf";
    UrlOpenResult result = processWebLink(url, true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertEquals(url, result.urlString());
    assertNotNull(result.uri());
    assertEquals("file", result.uri().getScheme());
    assertEquals("server.local", result.uri().getHost());
    assertEquals("/share/documents/file.pdf", result.uri().getPath());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_FileUrl_WithTrailingSlash()
  {
    String url = "file:///C:/Windows/";
    UrlOpenResult result = processWebLink(url, true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertNotNull(result.uri());
    assertEquals("file", result.uri().getScheme());
    assertTrue(result.uri().getPath().endsWith("/"));
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_FileUrl_WithEncodedSpaces()
  {
    String url = "file://server.example.com/files/Docs/Department/Reports/Project%20Status%20List.xlsx";
    UrlOpenResult result = processWebLink(url, true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertNotNull(result.uri());
    assertEquals("file", result.uri().getScheme());
    assertEquals("server.example.com", result.uri().getHost());
    assertNotDoubleEncoded(url, result.uri());
    assertNull(result.errorMessage());
  }

  // === BROWSE_WEB action tests: OneNote URLs ===

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_OneNote_SimpleWithFragment()
  {
    String url = "onenote://notebook/section.one#PageTitle";
    UrlOpenResult result = processWebLink(url, true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertEquals(url, result.urlString());
    assertNotNull(result.uri());
    assertEquals("onenote", result.uri().getScheme());
    assertEquals("PageTitle", result.uri().getFragment());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_OneNote_ComplexWithSectionAndPageIds()
  {
    String url = "onenote://server/notebooks/Work%20Notes.one#Meeting%20Notes&section-id={1A2B3C4D-5E6F-7A8B-9C0D-E1F2A3B4C5D6}&page-id={2B3C4D5E-6F7A-8B9C-0D1E-F2A3B4C5D6E7}&end";
    UrlOpenResult result = processWebLink(url, true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertEquals(url, result.urlString());
    assertNotNull(result.uri());
    assertEquals("onenote", result.uri().getScheme());
    assertNotNull(result.uri().getFragment());
    assertTrue(result.uri().getFragment().contains("section-id"));
    assertTrue(result.uri().getFragment().contains("page-id"));
    assertNotDoubleEncoded(url, result.uri());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_OneNote_TripleSlashWithDrivePath()
  {
    String url = "onenote:///C:/Users/Test/Documents/Notebook.one#Section&section-id={3C4D5E6F-7A8B-9C0D-1E2F-A3B4C5D6E7F8}&end";
    UrlOpenResult result = processWebLink(url, true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertEquals(url, result.urlString());
    assertNotNull(result.uri());
    assertEquals("onenote", result.uri().getScheme());
    assertNotDoubleEncoded(url, result.uri());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_OneNote_TripleSlashWithBackslashesAndAmpersand()
  {
    // OneNote URL with Windows backslashes and ampersand in path
    String url = "onenote:///F:\\DEPT\\SECTION\\Accounting\\M&A%20Excursion%20operations\\AccountingNotebook\\2023.one#section-id={3C4D5E6F-7A8B-9C0D-1E2F-A3B4C5D6E7F8}&end";
    UrlOpenResult result = processWebLink(url, true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertEquals(url, result.urlString());
    assertNotNull(result.uri());
    assertEquals("onenote", result.uri().getScheme());
    assertNotNull(result.uri().getFragment());
    assertTrue(result.uri().getFragment().contains("section-id"));
    assertNotDoubleEncoded(url, result.uri());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_OneNote_AmpersandInPath()
  {
    String url = "onenote://server.example.com/files/docs/Department/Agenda/M&A%20Meeting%20Notes/Meeting/Notes.one#October%202023&section-id=%7B7A8B9C0D-1E2F-3A4B-5C6D-E7F8A9B0C1D2%7D&page-id=%7B8B9C0D1E-2F3A-4B5C-6D7E-F8A9B0C1D2E3%7D&end";
    UrlOpenResult result = processWebLink(url, true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertEquals(url, result.urlString());
    assertNotNull(result.uri());
    assertEquals("onenote", result.uri().getScheme());
    assertNotNull(result.uri().getFragment());
    assertNotDoubleEncoded(url, result.uri());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_OneNote_SpacesInFragment()
  {
    // OneNote URLs often have pre-encoded spaces in the fragment (page titles, etc.)
    String url = "onenote://F/Department/Shared/Repository/Projects/%5b12345%5d%20Project%20Name.one#Yearly%20review%20meeting&section-id={4D5E6F7A-8B9C-0D1E-2F3A-B4C5D6E7F8A9}&page-id={5E6F7A8B-9C0D-1E2F-3A4B-C5D6E7F8A9B0}&end";
    UrlOpenResult result = processWebLink(url, true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertNotNull(result.uri());
    assertEquals("onenote", result.uri().getScheme());
    assertNotNull(result.uri().getFragment());
    assertNotDoubleEncoded(url, result.uri());
    assertNull(result.errorMessage());
  }

  // === BROWSE_WEB action tests: ext1:// URLs ===

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_Ext1_WithExtPathAvailable()
  {
    UrlOpenResult result = processWebLink("ext1://documents/papers/article.pdf", true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertEquals("ext1://documents/papers/article.pdf", result.urlString());
    assertNotNull(result.uri());
    assertEquals("ext1", result.uri().getScheme());
    assertNull(result.errorMessage());
  }

  // === BROWSE_WEB action tests: Whitespace handling ===

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_LeadingWhitespace()
  {
    UrlOpenResult result = processWebLink("   http://example.com", true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertEquals("http://example.com", result.urlString());
    assertNotNull(result.uri());
    assertEquals("example.com", result.uri().getHost());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_TrailingWhitespace()
  {
    UrlOpenResult result = processWebLink("http://example.com   ", true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertEquals("http://example.com", result.urlString());
    assertNotNull(result.uri());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_LeadingAndTrailingWhitespace()
  {
    UrlOpenResult result = processWebLink("  \t http://example.com/path \n  ", true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertEquals("http://example.com/path", result.urlString());
    assertNotNull(result.uri());
    assertEquals("/path", result.uri().getPath());
    assertNull(result.errorMessage());
  }

  // === BROWSE_WEB action tests: Complex real-world URLs ===

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_SharePointUrl_Complex()
  {
    String url = "https://contoso.sharepoint.com/sites/TeamSite/Shared%20Documents/Reports/Q4%20Report.xlsx";
    UrlOpenResult result = processWebLink(url, true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertEquals(url, result.urlString());
    assertNotNull(result.uri());
    assertEquals("https", result.uri().getScheme());
    assertEquals("contoso.sharepoint.com", result.uri().getHost());
    assertNotDoubleEncoded(url, result.uri());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_SharePointDocumentUrl_ColonWPattern()
  {
    // SharePoint document sharing URLs use :w: for Word, :x: for Excel, etc.
    String url = "https://company.sharepoint.com/:w:/s/TeamSite/AbCdEfGhIjKlMnOp-QrStUv-WxYzAbCdEfGhIjKlMnOpQrSt?e=XyZ123";
    UrlOpenResult result = processWebLink(url, true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertEquals(url, result.urlString());
    assertNotNull(result.uri());
    assertEquals("https", result.uri().getScheme());
    assertEquals("company.sharepoint.com", result.uri().getHost());
    assertEquals("/:w:/s/TeamSite/AbCdEfGhIjKlMnOp-QrStUv-WxYzAbCdEfGhIjKlMnOpQrSt", result.uri().getPath());
    assertEquals("e=XyZ123", result.uri().getQuery());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_InternalServerWithoutTld()
  {
    // Internal servers often don't have a TLD (e.g., https://intranet/path)
    String url = "https://dnc3summary/SummaryReport.ashx/home/XSERVER/Project%20Status%20Report/P-123456-V-Nov25";
    UrlOpenResult result = processWebLink(url, true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertEquals(url, result.urlString());
    assertNotNull(result.uri());
    assertEquals("https", result.uri().getScheme());
    assertEquals("dnc3summary", result.uri().getHost());
    assertNotDoubleEncoded(url, result.uri());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_GoogleDocsUrl()
  {
    String url = "https://docs.google.com/document/d/1aBcDeFgHiJkLmNoPqRsTuVwXyZ/edit?usp=sharing#heading=h.abc123";
    UrlOpenResult result = processWebLink(url, true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertEquals(url, result.urlString());
    assertNotNull(result.uri());
    assertEquals("https", result.uri().getScheme());
    assertEquals("docs.google.com", result.uri().getHost());
    assertEquals("usp=sharing", result.uri().getQuery());
    assertEquals("heading=h.abc123", result.uri().getFragment());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_GitHubUrl_WithFragment()
  {
    String url = "https://github.com/owner/repo/blob/main/src/file.java#L42-L50";
    UrlOpenResult result = processWebLink(url, true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertEquals(url, result.urlString());
    assertNotNull(result.uri());
    assertEquals("https", result.uri().getScheme());
    assertEquals("github.com", result.uri().getHost());
    assertEquals("/owner/repo/blob/main/src/file.java", result.uri().getPath());
    assertEquals("L42-L50", result.uri().getFragment());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_ChromeTextFragment_PreEncoded_Ticket72()
  {
    // Regression test for SourceForge ticket #72:
    // Chrome's "link to highlighted text" feature creates URLs with pre-encoded fragments.
    // These must NOT be double-encoded (%20 must not become %2520).
    String url = "https://en.wikipedia.org/wiki/Fish_jaw#:~:text=the%20444%2Dmillion";
    UrlOpenResult result = processWebLink(url, true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertNotNull(result.uri());
    assertEquals("en.wikipedia.org", result.uri().getHost());
    assertEquals("/wiki/Fish_jaw", result.uri().getPath());

    // Critical assertion: pre-encoded %20 must stay as %20, not become %2520
    String rawFragment = result.uri().getRawFragment();
    assertTrue(rawFragment.contains("%20"), "Pre-encoded %20 should be preserved");
    assertFalse(rawFragment.contains("%2520"), "Must not double-encode %20 to %2520");
    assertEquals(":~:text=the%20444%2Dmillion", rawFragment);
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_IPv6_BracketsInHostPreserved()
  {
    // IPv6 addresses use brackets in the host; these must NOT be escaped
    UrlOpenResult result = processWebLink("http://[::1]:8080/path", true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertNotNull(result.uri());
    assertEquals("[::1]", result.uri().getHost());
    assertEquals(8080, result.uri().getPort());
    assertEquals("/path", result.uri().getPath());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_IPv6_WithBracketsInPath()
  {
    // IPv6 host brackets preserved, but brackets in path should be escaped
    UrlOpenResult result = processWebLink("http://[::1]/path[0]/file", true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertNotNull(result.uri());
    assertEquals("[::1]", result.uri().getHost());
    assertEquals("/path%5B0%5D/file", result.uri().getRawPath());
    assertEquals("/path[0]/file", result.uri().getPath());  // getPath() decodes
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_IPv6_QueryNoPath_BracketsInQuery()
  {
    // Edge case: IPv6 host, query with brackets, but no path
    UrlOpenResult result = processWebLink("http://[::1]?filter=[active]", true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertNotNull(result.uri());
    assertEquals("[::1]", result.uri().getHost());
    assertEquals("", result.uri().getPath());
    assertEquals("filter=%5Bactive%5D", result.uri().getRawQuery());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_IPv6_FragmentNoPath_BracketsInFragment()
  {
    // Edge case: IPv6 host, fragment with brackets, but no path
    UrlOpenResult result = processWebLink("http://[::1]#section[0]", true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertNotNull(result.uri());
    assertEquals("[::1]", result.uri().getHost());
    assertEquals("", result.uri().getPath());
    assertEquals("section%5B0%5D", result.uri().getRawFragment());
    assertNull(result.errorMessage());
  }

//---------------------------------------------------------------------------

  // Tests for all illegal printable ASCII characters per RFC 3986
  // These verify the escapeIllegalUriChars method handles each character

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_IllegalChar_Caret()
  {
    UrlOpenResult result = processWebLink("https://example.com/path^value", true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertNotNull(result.uri());
    assertEquals("/path%5Evalue", result.uri().getRawPath());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_IllegalChar_Backtick()
  {
    UrlOpenResult result = processWebLink("https://example.com/path`value", true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertNotNull(result.uri());
    assertEquals("/path%60value", result.uri().getRawPath());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_IllegalChar_LessThan()
  {
    UrlOpenResult result = processWebLink("https://example.com/path<value", true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertNotNull(result.uri());
    assertEquals("/path%3Cvalue", result.uri().getRawPath());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_IllegalChar_GreaterThan()
  {
    UrlOpenResult result = processWebLink("https://example.com/path>value", true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertNotNull(result.uri());
    assertEquals("/path%3Evalue", result.uri().getRawPath());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_IllegalChar_DoubleQuote()
  {
    UrlOpenResult result = processWebLink("https://example.com/path\"value", true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertNotNull(result.uri());
    assertEquals("/path%22value", result.uri().getRawPath());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_IllegalChar_CurlyBraces()
  {
    UrlOpenResult result = processWebLink("https://example.com/path{value}", true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertNotNull(result.uri());
    assertEquals("/path%7Bvalue%7D", result.uri().getRawPath());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_IllegalChar_Pipe()
  {
    UrlOpenResult result = processWebLink("https://example.com/path|value", true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertNotNull(result.uri());
    assertEquals("/path%7Cvalue", result.uri().getRawPath());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_IllegalChar_Backslash()
  {
    UrlOpenResult result = processWebLink("https://example.com/path\\value", true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertNotNull(result.uri());
    assertEquals("/path%5Cvalue", result.uri().getRawPath());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_IllegalChars_AllInOne()
  {
    // URL with all illegal printable ASCII chars: " < > \ ^ ` { | }
    UrlOpenResult result = processWebLink("https://example.com/a\"b<c>d\\e^f`g{h|i}j", true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertNotNull(result.uri());
    assertEquals("/a%22b%3Cc%3Ed%5Ce%5Ef%60g%7Bh%7Ci%7Dj", result.uri().getRawPath());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_IllegalChars_InFragment()
  {
    // Illegal chars in fragment should also be escaped
    UrlOpenResult result = processWebLink("https://example.com/path#section{id}", true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertNotNull(result.uri());
    assertEquals("section%7Bid%7D", result.uri().getRawFragment());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_IllegalChars_PreEncodedPlusNew()
  {
    // Pre-encoded %20 should not be double-encoded even with other illegal chars present
    UrlOpenResult result = processWebLink("https://example.com/path%20with%20spaces{and}braces", true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertNotNull(result.uri());
    String rawPath = result.uri().getRawPath();
    assertTrue(rawPath.contains("%20"), "Pre-encoded spaces should remain as %20");
    assertFalse(rawPath.contains("%2520"), "Should not double-encode %20");
    assertTrue(rawPath.contains("%7B") && rawPath.contains("%7D"), "Curly braces should be escaped");
    assertNull(result.errorMessage());
  }

//---------------------------------------------------------------------------

  // Tests for square bracket handling: brackets are escaped in path/query/fragment
  // but preserved in host portion for IPv6 addresses.

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_SquareBrackets_InPath_NoFragment()
  {
    // Brackets in path are escaped to %5B/%5D
    UrlOpenResult result = processWebLink("https://example.com/path[0]/file", true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertNotNull(result.uri());
    assertEquals("https", result.uri().getScheme());
    assertEquals("example.com", result.uri().getHost());
    assertEquals("/path%5B0%5D/file", result.uri().getRawPath());
    assertEquals("/path[0]/file", result.uri().getPath());  // getPath() decodes
    assertNull(result.uri().getFragment());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_SquareBrackets_InPath_WithFragment()
  {
    // Brackets in path are escaped, fragment is preserved
    UrlOpenResult result = processWebLink("https://example.com/path[0]/file#section", true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertNotNull(result.uri());
    assertEquals("https", result.uri().getScheme());
    assertEquals("example.com", result.uri().getHost());
    assertEquals("/path%5B0%5D/file", result.uri().getRawPath());
    assertEquals("section", result.uri().getFragment());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_SquareBrackets_InFragment_Only()
  {
    // Brackets are escaped in path/query/fragment (only preserved in host for IPv6).
    // Encoded brackets produce valid URIs that work correctly.
    UrlOpenResult result = processWebLink("https://example.com/path#section[0]", true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertNotNull(result.uri());
    assertEquals("/path", result.uri().getPath());
    assertEquals("section[0]", result.uri().getFragment());  // getFragment() decodes
    assertEquals("section%5B0%5D", result.uri().getRawFragment());  // Raw shows encoding
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_SquareBrackets_InQuery()
  {
    // Brackets in query are escaped to %5B/%5D; getQuery() decodes them back
    UrlOpenResult result = processWebLink("https://example.com/path?filter=[active]", true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertNotNull(result.uri());
    assertEquals("/path", result.uri().getPath());
    assertEquals("filter=%5Bactive%5D", result.uri().getRawQuery());  // Raw shows encoding
    assertEquals("filter=[active]", result.uri().getQuery());  // getQuery() decodes
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_SquareBrackets_PreEncoded()
  {
    // Pre-encoded brackets should not be double-encoded
    UrlOpenResult result = processWebLink("https://example.com/path%5B0%5D/file", true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertNotNull(result.uri());
    assertEquals("/path%5B0%5D/file", result.uri().getRawPath());
    assertFalse(result.uri().getRawPath().contains("%255B"), "Should not double-encode brackets");
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_ComplexUrl_AllComponents()
  {
    // Complex URL with all components: userinfo, port, path with brackets, query, fragment
    UrlOpenResult result = processWebLink("https://user:pass@example.com:8080/path[0]/file?q=1#frag", true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertNotNull(result.uri());
    assertEquals("https", result.uri().getScheme());
    assertEquals("example.com", result.uri().getHost());
    assertEquals(8080, result.uri().getPort());
    assertEquals("user:pass", result.uri().getUserInfo());
    assertEquals("/path%5B0%5D/file", result.uri().getRawPath());
    assertEquals("q=1", result.uri().getQuery());
    assertEquals("frag", result.uri().getFragment());
    assertNull(result.errorMessage());
  }

//---------------------------------------------------------------------------

  // Additional edge case tests

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_RawSpaceInPath()
  {
    // Raw (unescaped) space in path should be escaped to %20
    UrlOpenResult result = processWebLink("https://example.com/my file.txt", true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertNotNull(result.uri());
    assertEquals("/my%20file.txt", result.uri().getRawPath());
    assertEquals("/my file.txt", result.uri().getPath());  // getPath() decodes
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_RawSpaceInQuery()
  {
    // Raw space in query should be escaped to %20
    UrlOpenResult result = processWebLink("https://example.com/search?q=hello world", true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertNotNull(result.uri());
    assertEquals("/search", result.uri().getPath());
    assertEquals("q=hello%20world", result.uri().getRawQuery());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_RawSpaceInFragment()
  {
    // Raw space in fragment should be escaped to %20
    UrlOpenResult result = processWebLink("https://example.com/page#section two", true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertNotNull(result.uri());
    assertEquals("section%20two", result.uri().getRawFragment());
    assertEquals("section two", result.uri().getFragment());  // getFragment() decodes
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_MalformedPercent_InvalidHex()
  {
    // %ZZ is not valid hex; URI constructor will reject it
    UrlOpenResult result = processWebLink("https://example.com/100%ZZdiscount", true);

    assertEquals(UrlOpenAction.INVALID, result.action());
    assertEquals("https://example.com/100%ZZdiscount", result.urlString());
    assertNull(result.uri());
    assertNotNull(result.errorMessage(), "Should have error message for invalid URI");
    assertTrue(result.errorMessage().contains("error"), "Error message should indicate an error occurred");
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_MalformedPercent_InvalidSecondHexDigit()
  {
    // %2g has invalid second hex dig; URI constructor will reject it
    UrlOpenResult result = processWebLink("https://example.com/100%2gdiscount", true);

    assertEquals(UrlOpenAction.INVALID, result.action());
    assertEquals("https://example.com/100%2gdiscount", result.urlString());
    assertNull(result.uri());
    assertNotNull(result.errorMessage(), "Should have error message for invalid URI");
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_MalformedPercent_Incomplete()
  {
    // %2 with only one hex digit is incomplete; URI constructor will reject it
    UrlOpenResult result = processWebLink("https://example.com/100%2", true);

    assertEquals(UrlOpenAction.INVALID, result.action());
    assertEquals("https://example.com/100%2", result.urlString());
    assertNull(result.uri());
    assertNotNull(result.errorMessage(), "Should have error message for invalid URI");
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_MalformedPercent_AtEnd()
  {
    // Trailing % with nothing after; URI constructor will reject it
    UrlOpenResult result = processWebLink("https://example.com/100%", true);

    assertEquals(UrlOpenAction.INVALID, result.action());
    assertNotNull(result.errorMessage(), "Should have error message for invalid URI");
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_EscapedPercent_NoDoubleEncode()
  {
    // %25 is an escaped percent sign; should NOT become %2525
    UrlOpenResult result = processWebLink("https://example.com/100%25discount", true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertNotNull(result.uri());
    assertTrue(result.uri().getRawPath().contains("%25"), "Escaped percent should remain as %25");
    assertFalse(result.uri().getRawPath().contains("%2525"), "Should not double-encode %25 to %2525");
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_IllegalChars_InPath_AllChars()
  {
    // All illegal chars in path: " < > \ ^ ` { | } [ ]
    UrlOpenResult result = processWebLink("https://example.com/a\"b<c>d\\e^f`g{h|i}j[k]l", true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertNotNull(result.uri());
    String rawPath = result.uri().getRawPath();
    assertTrue(rawPath.contains("%22"), "Double quote should be escaped");
    assertTrue(rawPath.contains("%3C"), "Less than should be escaped");
    assertTrue(rawPath.contains("%3E"), "Greater than should be escaped");
    assertTrue(rawPath.contains("%5C"), "Backslash should be escaped");
    assertTrue(rawPath.contains("%5E"), "Caret should be escaped");
    assertTrue(rawPath.contains("%60"), "Backtick should be escaped");
    assertTrue(rawPath.contains("%7B"), "Open brace should be escaped");
    assertTrue(rawPath.contains("%7C"), "Pipe should be escaped");
    assertTrue(rawPath.contains("%7D"), "Close brace should be escaped");
    assertTrue(rawPath.contains("%5B"), "Open bracket should be escaped");
    assertTrue(rawPath.contains("%5D"), "Close bracket should be escaped");
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_IllegalChars_InQuery_AllChars()
  {
    // All illegal chars in query string: " < > \ ^ ` { | } [ ]
    UrlOpenResult result = processWebLink("https://example.com/path?a=\"b<c>d\\e^f`g{h|i}j[k]l", true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertNotNull(result.uri());
    String rawQuery = result.uri().getRawQuery();
    assertTrue(rawQuery.contains("%22"), "Double quote should be escaped");
    assertTrue(rawQuery.contains("%3C"), "Less than should be escaped");
    assertTrue(rawQuery.contains("%3E"), "Greater than should be escaped");
    assertTrue(rawQuery.contains("%5C"), "Backslash should be escaped");
    assertTrue(rawQuery.contains("%5E"), "Caret should be escaped");
    assertTrue(rawQuery.contains("%60"), "Backtick should be escaped");
    assertTrue(rawQuery.contains("%7B"), "Open brace should be escaped");
    assertTrue(rawQuery.contains("%7C"), "Pipe should be escaped");
    assertTrue(rawQuery.contains("%7D"), "Close brace should be escaped");
    assertTrue(rawQuery.contains("%5B"), "Open bracket should be escaped");
    assertTrue(rawQuery.contains("%5D"), "Close bracket should be escaped");
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_IllegalChars_InFragment_AllChars()
  {
    // All illegal chars in fragment: " < > \ ^ ` { | } [ ]
    UrlOpenResult result = processWebLink("https://example.com/path#a\"b<c>d\\e^f`g{h|i}j[k]l", true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertNotNull(result.uri());
    String rawFragment = result.uri().getRawFragment();
    assertTrue(rawFragment.contains("%22"), "Double quote should be escaped");
    assertTrue(rawFragment.contains("%3C"), "Less than should be escaped");
    assertTrue(rawFragment.contains("%3E"), "Greater than should be escaped");
    assertTrue(rawFragment.contains("%5C"), "Backslash should be escaped");
    assertTrue(rawFragment.contains("%5E"), "Caret should be escaped");
    assertTrue(rawFragment.contains("%60"), "Backtick should be escaped");
    assertTrue(rawFragment.contains("%7B"), "Open brace should be escaped");
    assertTrue(rawFragment.contains("%7C"), "Pipe should be escaped");
    assertTrue(rawFragment.contains("%7D"), "Close brace should be escaped");
    assertTrue(rawFragment.contains("%5B"), "Open bracket should be escaped");
    assertTrue(rawFragment.contains("%5D"), "Close bracket should be escaped");
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_EmptyPathWithQuery_RegularHost()
  {
    // URL with query but no path (empty path)
    UrlOpenResult result = processWebLink("https://example.com?query=value", true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertNotNull(result.uri());
    assertEquals("https", result.uri().getScheme());
    assertEquals("example.com", result.uri().getHost());
    assertEquals("", result.uri().getPath());
    assertEquals("query=value", result.uri().getQuery());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_EmptyPathWithFragment_RegularHost()
  {
    // URL with fragment but no path (empty path)
    UrlOpenResult result = processWebLink("https://example.com#section", true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertNotNull(result.uri());
    assertEquals("https", result.uri().getScheme());
    assertEquals("example.com", result.uri().getHost());
    assertEquals("", result.uri().getPath());
    assertEquals("section", result.uri().getFragment());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs(org.junit.jupiter.api.condition.OS.LINUX)
  void testProcessWebLink_Linux_ReturnsNullUri()
  {
    // On Linux, processWebLink returns null URI (uses openSystemSpecific instead)
    UrlOpenResult result = processWebLink("https://example.com/path", true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertEquals("https://example.com/path", result.urlString());
    assertNull(result.uri(), "Linux should return null URI");
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs(org.junit.jupiter.api.condition.OS.LINUX)
  void testProcessWebLink_Linux_PlainDomain()
  {
    // On Linux, plain domain gets http:// prepended but no URI constructed
    UrlOpenResult result = processWebLink("example.com", true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertEquals("http://example.com", result.urlString());
    assertNull(result.uri(), "Linux should return null URI");
    assertNull(result.errorMessage());
  }

  // === LAUNCH_FILE action tests (Mac/Linux Unix paths) ===

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.MAC, org.junit.jupiter.api.condition.OS.LINUX})
  void testProcessWebLink_UnixAbsolutePath_Simple()
  {
    UrlOpenResult result = processWebLink("/home/user/file.txt", true);

    assertEquals(UrlOpenAction.LAUNCH_FILE, result.action());
    assertEquals("/home/user/file.txt", result.urlString());
    assertNull(result.uri(), "URI should be null for LAUNCH_FILE");
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.MAC, org.junit.jupiter.api.condition.OS.LINUX})
  void testProcessWebLink_UnixAbsolutePath_WithSpaces()
  {
    UrlOpenResult result = processWebLink("/home/user/My Documents/file.txt", true);

    assertEquals(UrlOpenAction.LAUNCH_FILE, result.action());
    assertEquals("/home/user/My Documents/file.txt", result.urlString());
    assertNull(result.uri());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.MAC, org.junit.jupiter.api.condition.OS.LINUX})
  void testProcessWebLink_UnixAbsolutePath_WithSpecialChars()
  {
    UrlOpenResult result = processWebLink("/home/user/Report (2024) - Final.pdf", true);

    assertEquals(UrlOpenAction.LAUNCH_FILE, result.action());
    assertEquals("/home/user/Report (2024) - Final.pdf", result.urlString());
    assertNull(result.uri());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.MAC, org.junit.jupiter.api.condition.OS.LINUX})
  void testProcessWebLink_UnixAbsolutePath_WithColonInFilename()
  {
    // Colons are valid in Unix filenames; should not be mistaken for URL scheme
    UrlOpenResult result = processWebLink("/home/user/Meeting: Notes.txt", true);

    assertEquals(UrlOpenAction.LAUNCH_FILE, result.action());
    assertEquals("/home/user/Meeting: Notes.txt", result.urlString());
    assertNull(result.uri());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.MAC, org.junit.jupiter.api.condition.OS.LINUX})
  void testProcessWebLink_UnixAbsolutePath_WithMultipleColons()
  {
    // Multiple colons in path; still a valid Unix path
    UrlOpenResult result = processWebLink("/home/user/2024-01-15: Meeting: Q1 Review.txt", true);

    assertEquals(UrlOpenAction.LAUNCH_FILE, result.action());
    assertEquals("/home/user/2024-01-15: Meeting: Q1 Review.txt", result.urlString());
    assertNull(result.uri());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs(org.junit.jupiter.api.condition.OS.MAC)
  void testProcessWebLink_MacOsPath_Users()
  {
    UrlOpenResult result = processWebLink("/Users/name/Documents/paper.pdf", true);

    assertEquals(UrlOpenAction.LAUNCH_FILE, result.action());
    assertEquals("/Users/name/Documents/paper.pdf", result.urlString());
    assertNull(result.uri());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs(org.junit.jupiter.api.condition.OS.MAC)
  void testProcessWebLink_MacOsPath_Applications()
  {
    UrlOpenResult result = processWebLink("/Applications/Safari.app", true);

    assertEquals(UrlOpenAction.LAUNCH_FILE, result.action());
    assertEquals("/Applications/Safari.app", result.urlString());
    assertNull(result.uri());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs(org.junit.jupiter.api.condition.OS.MAC)
  void testProcessWebLink_MacOsPath_Volumes()
  {
    // External drives on Mac appear under /Volumes
    UrlOpenResult result = processWebLink("/Volumes/External Drive/Documents/file.pdf", true);

    assertEquals(UrlOpenAction.LAUNCH_FILE, result.action());
    assertEquals("/Volumes/External Drive/Documents/file.pdf", result.urlString());
    assertNull(result.uri());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs(org.junit.jupiter.api.condition.OS.LINUX)
  void testProcessWebLink_LinuxPath_Home()
  {
    UrlOpenResult result = processWebLink("/home/username/Documents/report.pdf", true);

    assertEquals(UrlOpenAction.LAUNCH_FILE, result.action());
    assertEquals("/home/username/Documents/report.pdf", result.urlString());
    assertNull(result.uri());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs(org.junit.jupiter.api.condition.OS.LINUX)
  void testProcessWebLink_LinuxPath_Mnt()
  {
    // Mounted drives on Linux often appear under /mnt
    UrlOpenResult result = processWebLink("/mnt/usb/Documents/file.txt", true);

    assertEquals(UrlOpenAction.LAUNCH_FILE, result.action());
    assertEquals("/mnt/usb/Documents/file.txt", result.urlString());
    assertNull(result.uri());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs(org.junit.jupiter.api.condition.OS.LINUX)
  void testProcessWebLink_LinuxPath_Media()
  {
    // Auto-mounted drives on Linux often appear under /media
    UrlOpenResult result = processWebLink("/media/username/USB Drive/file.pdf", true);

    assertEquals(UrlOpenAction.LAUNCH_FILE, result.action());
    assertEquals("/media/username/USB Drive/file.pdf", result.urlString());
    assertNull(result.uri());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.MAC, org.junit.jupiter.api.condition.OS.LINUX})
  void testProcessWebLink_UnixPath_Root()
  {
    // Root directory
    UrlOpenResult result = processWebLink("/", true);

    assertEquals(UrlOpenAction.LAUNCH_FILE, result.action());
    assertEquals("/", result.urlString());
    assertNull(result.uri());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.MAC, org.junit.jupiter.api.condition.OS.LINUX})
  void testProcessWebLink_UnixPath_SingleComponent()
  {
    // Single directory like /tmp
    UrlOpenResult result = processWebLink("/tmp", true);

    assertEquals(UrlOpenAction.LAUNCH_FILE, result.action());
    assertEquals("/tmp", result.urlString());
    assertNull(result.uri());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.MAC, org.junit.jupiter.api.condition.OS.LINUX})
  void testProcessWebLink_UnixPath_Etc()
  {
    UrlOpenResult result = processWebLink("/etc/hosts", true);

    assertEquals(UrlOpenAction.LAUNCH_FILE, result.action());
    assertEquals("/etc/hosts", result.urlString());
    assertNull(result.uri());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.MAC, org.junit.jupiter.api.condition.OS.LINUX})
  void testProcessWebLink_UnixPath_Var()
  {
    UrlOpenResult result = processWebLink("/var/log/syslog", true);

    assertEquals(UrlOpenAction.LAUNCH_FILE, result.action());
    assertEquals("/var/log/syslog", result.urlString());
    assertNull(result.uri());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.MAC, org.junit.jupiter.api.condition.OS.LINUX})
  void testProcessWebLink_UnixPath_WithBrackets()
  {
    // Brackets are valid in Unix filenames
    UrlOpenResult result = processWebLink("/home/user/[Project] Notes.txt", true);

    assertEquals(UrlOpenAction.LAUNCH_FILE, result.action());
    assertEquals("/home/user/[Project] Notes.txt", result.urlString());
    assertNull(result.uri());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.MAC, org.junit.jupiter.api.condition.OS.LINUX})
  void testProcessWebLink_UnixPath_WithCurlyBraces()
  {
    // Curly braces are valid in Unix filenames
    UrlOpenResult result = processWebLink("/home/user/file{1}.txt", true);

    assertEquals(UrlOpenAction.LAUNCH_FILE, result.action());
    assertEquals("/home/user/file{1}.txt", result.urlString());
    assertNull(result.uri());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.MAC, org.junit.jupiter.api.condition.OS.LINUX})
  void testProcessWebLink_UnixPath_WithUnicodeChars()
  {
    // Unicode characters in Unix paths
    UrlOpenResult result = processWebLink("/home/user/文档/файл.txt", true);

    assertEquals(UrlOpenAction.LAUNCH_FILE, result.action());
    assertEquals("/home/user/文档/файл.txt", result.urlString());
    assertNull(result.uri());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.MAC, org.junit.jupiter.api.condition.OS.LINUX})
  void testProcessWebLink_UnixPath_HiddenFile()
  {
    // Hidden files (starting with .) in Unix
    UrlOpenResult result = processWebLink("/home/user/.config/app/settings.json", true);

    assertEquals(UrlOpenAction.LAUNCH_FILE, result.action());
    assertEquals("/home/user/.config/app/settings.json", result.urlString());
    assertNull(result.uri());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.MAC, org.junit.jupiter.api.condition.OS.LINUX})
  void testProcessWebLink_UnixPath_TrailingSlash()
  {
    // Path with trailing slash (directory)
    UrlOpenResult result = processWebLink("/home/user/Documents/", true);

    assertEquals(UrlOpenAction.LAUNCH_FILE, result.action());
    assertEquals("/home/user/Documents/", result.urlString());
    assertNull(result.uri());
    assertNull(result.errorMessage());
  }

  // === Tilde path tests (Mac/Linux home directory expansion) ===

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.MAC, org.junit.jupiter.api.condition.OS.LINUX})
  void testProcessWebLink_TildePath_Simple()
  {
    UrlOpenResult result = processWebLink("~/Documents/file.txt", true);

    assertEquals(UrlOpenAction.LAUNCH_FILE, result.action());
    String home = System.getProperty("user.home");
    assertEquals(home + "/Documents/file.txt", result.urlString());
    assertNull(result.uri());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.MAC, org.junit.jupiter.api.condition.OS.LINUX})
  void testProcessWebLink_TildePath_WithSpaces()
  {
    UrlOpenResult result = processWebLink("~/My Documents/report.pdf", true);

    assertEquals(UrlOpenAction.LAUNCH_FILE, result.action());
    String home = System.getProperty("user.home");
    assertEquals(home + "/My Documents/report.pdf", result.urlString());
    assertNull(result.uri());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.MAC, org.junit.jupiter.api.condition.OS.LINUX})
  void testProcessWebLink_TildePath_WithSpecialChars()
  {
    UrlOpenResult result = processWebLink("~/Documents/Report (2024) - Final.pdf", true);

    assertEquals(UrlOpenAction.LAUNCH_FILE, result.action());
    String home = System.getProperty("user.home");
    assertEquals(home + "/Documents/Report (2024) - Final.pdf", result.urlString());
    assertNull(result.uri());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.MAC, org.junit.jupiter.api.condition.OS.LINUX})
  void testProcessWebLink_TildePath_JustTilde()
  {
    // Just ~ should expand to home directory
    UrlOpenResult result = processWebLink("~", true);

    assertEquals(UrlOpenAction.LAUNCH_FILE, result.action());
    String home = System.getProperty("user.home");
    assertEquals(home, result.urlString());
    assertNull(result.uri());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.MAC, org.junit.jupiter.api.condition.OS.LINUX})
  void testProcessWebLink_TildePath_Desktop()
  {
    UrlOpenResult result = processWebLink("~/Desktop/screenshot.png", true);

    assertEquals(UrlOpenAction.LAUNCH_FILE, result.action());
    String home = System.getProperty("user.home");
    assertEquals(home + "/Desktop/screenshot.png", result.urlString());
    assertNull(result.uri());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.MAC, org.junit.jupiter.api.condition.OS.LINUX})
  void testProcessWebLink_TildePath_Downloads()
  {
    UrlOpenResult result = processWebLink("~/Downloads/installer.dmg", true);

    assertEquals(UrlOpenAction.LAUNCH_FILE, result.action());
    String home = System.getProperty("user.home");
    assertEquals(home + "/Downloads/installer.dmg", result.urlString());
    assertNull(result.uri());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.MAC, org.junit.jupiter.api.condition.OS.LINUX})
  void testProcessWebLink_TildePath_HiddenDirectory()
  {
    UrlOpenResult result = processWebLink("~/.ssh/config", true);

    assertEquals(UrlOpenAction.LAUNCH_FILE, result.action());
    String home = System.getProperty("user.home");
    assertEquals(home + "/.ssh/config", result.urlString());
    assertNull(result.uri());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.MAC, org.junit.jupiter.api.condition.OS.LINUX})
  void testProcessWebLink_TildePath_WithColonInFilename()
  {
    // Tilde path with colon in filename
    UrlOpenResult result = processWebLink("~/Documents/Meeting: Notes.txt", true);

    assertEquals(UrlOpenAction.LAUNCH_FILE, result.action());
    String home = System.getProperty("user.home");
    assertEquals(home + "/Documents/Meeting: Notes.txt", result.urlString());
    assertNull(result.uri());
    assertNull(result.errorMessage());
  }

  // === Edge case: Double slash (protocol-relative URL) should NOT be treated as Unix path ===

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.MAC, org.junit.jupiter.api.condition.OS.LINUX})
  void testProcessWebLink_DoubleSlash_NotUnixPath()
  {
    // //example.com could be a protocol-relative URL, not a Unix path
    // Current behavior: prepends http:// which creates invalid URL http:////example.com
    // This documents current behavior; may want to improve handling in future
    UrlOpenResult result = processWebLink("//example.com/path", true);

    // Should NOT be LAUNCH_FILE; double slash is not a Unix absolute path
    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertEquals("http:////example.com/path", result.urlString());
    assertNull(result.errorMessage());
  }

  @Test
  void testProcessWebLink_FtpScheme()
  {
    // Test non-HTTP scheme (ftp://)
    UrlOpenResult result = processWebLink("ftp://ftp.example.com/pub/files/readme.txt", true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertEquals("ftp://ftp.example.com/pub/files/readme.txt", result.urlString());
    // URI will be constructed on Windows/Mac, null on Linux
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_TelScheme()
  {
    // Test telephone scheme
    UrlOpenResult result = processWebLink("tel:+1-555-123-4567", true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertNotNull(result.uri());
    assertEquals("tel", result.uri().getScheme());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_MultipleSpaces()
  {
    // Multiple consecutive spaces should each be escaped
    UrlOpenResult result = processWebLink("https://example.com/path/file   name.txt", true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertNotNull(result.uri());
    assertEquals("/path/file%20%20%20name.txt", result.uri().getRawPath());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_UnicodeInPath()
  {
    // Unicode characters in path (already valid in URIs when properly encoded)
    UrlOpenResult result = processWebLink("https://example.com/путь/文件", true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertNotNull(result.uri());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_ColonInQuery()
  {
    // WorldCat-style URLs use colons in query parameters (e.g., q=ti:test)
    String url = "http://www.worldcat.org/search?q=ti:test";
    UrlOpenResult result = processWebLink(url, true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertNotNull(result.uri());
    assertEquals("http", result.uri().getScheme());
    assertEquals("www.worldcat.org", result.uri().getHost());
    assertEquals("/search", result.uri().getPath());
    assertEquals("q=ti:test", result.uri().getQuery());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_SlashInFragment()
  {
    // Slashes are valid in fragments
    String url = "http://example.com#section/subsection";
    UrlOpenResult result = processWebLink(url, true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertNotNull(result.uri());
    assertEquals("http", result.uri().getScheme());
    assertEquals("section/subsection", result.uri().getFragment());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_DashesAndUnderscoresInPath()
  {
    // Dashes and underscores are valid unreserved characters in URIs
    String url = "http://example.com/path/with-dashes_and_underscores";
    UrlOpenResult result = processWebLink(url, true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertNotNull(result.uri());
    assertEquals("http", result.uri().getScheme());
    assertEquals("/path/with-dashes_and_underscores", result.uri().getPath());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_TildeInPath()
  {
    // Tilde is a valid unreserved character in URIs (common in Unix home directories)
    String url = "http://example.com/~user/page";
    UrlOpenResult result = processWebLink(url, true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertNotNull(result.uri());
    assertEquals("http", result.uri().getScheme());
    assertEquals("/~user/page", result.uri().getPath());
    assertNull(result.errorMessage());
  }

  // === Edge case URL structure tests ===

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_SchemeOnly()
  {
    // URL with scheme but nothing after the colon; URI constructor rejects this
    UrlOpenResult result = processWebLink("http:", true);

    assertEquals(UrlOpenAction.INVALID, result.action());
    assertEquals("http:", result.urlString());
    assertNull(result.uri());
    assertNotNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_SchemeWithSingleSlash()
  {
    // URL with scheme and single slash; valid URI with opaque scheme-specific part
    UrlOpenResult result = processWebLink("http:/path", true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertNotNull(result.uri());
    assertEquals("http", result.uri().getScheme());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_SchemeWithDoubleSlashOnly()
  {
    // URL with scheme and // but no authority; URI constructor rejects this
    UrlOpenResult result = processWebLink("http://", true);

    assertEquals(UrlOpenAction.INVALID, result.action());
    assertEquals("http://", result.urlString());
    assertNull(result.uri());
    assertNotNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_SchemeWithEmptyAuthority_AndPath()
  {
    // http:///path - empty authority with path
    UrlOpenResult result = processWebLink("http:///path", true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertNotNull(result.uri());
    assertEquals("http", result.uri().getScheme());
    assertEquals("/path", result.uri().getPath());
    assertNull(result.errorMessage());
  }

  @Test
  void testProcessWebLink_NullInput()
  {
    // Null input throws NullPointerException; this documents expected behavior
    assertThrows(NullPointerException.class, () -> processWebLink(null, true));
  }

  @Test
  @EnabledOnOs(org.junit.jupiter.api.condition.OS.WINDOWS)
  void testProcessWebLink_Windows_UrlWithColonNotValidPath()
  {
    // On Windows, URLs with : that aren't valid file paths fall through to BROWSE_WEB.
    // This explicitly tests that code path (Paths.get throws InvalidPathException).
    UrlOpenResult result = processWebLink("http://example.com", true);

    // Should NOT be LAUNCH_FILE because "http://example.com" is not a valid Windows path
    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertNotNull(result.uri());
    assertEquals("http", result.uri().getScheme());
    assertEquals("example.com", result.uri().getHost());
    assertNull(result.errorMessage());
  }

  // === UNC path edge cases ===

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_UncPath_ServerOnly_NoShare()
  {
    // UNC path with server but no share name: \\server
    UrlOpenResult result = processWebLink("\\\\server", true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertEquals("file://server", result.urlString());
    assertNotNull(result.uri());
    assertEquals("file", result.uri().getScheme());
    assertEquals("server", result.uri().getHost());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_UncPath_JustBackslashes()
  {
    // Just two backslashes with nothing after: \\ becomes file:// which URI rejects
    UrlOpenResult result = processWebLink("\\\\", true);

    assertEquals(UrlOpenAction.INVALID, result.action());
    assertEquals("file://", result.urlString());
    assertNull(result.uri());
    assertNotNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_UncPath_ThreeBackslashes()
  {
    // Three backslashes: \\\server - first two trigger UNC handling, third becomes /
    UrlOpenResult result = processWebLink("\\\\\\server", true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertEquals("file:///server", result.urlString());
    assertNotNull(result.uri());
    assertEquals("file", result.uri().getScheme());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_UncPath_TrailingBackslash()
  {
    // UNC path with trailing backslash: \\server\share\
    UrlOpenResult result = processWebLink("\\\\server\\share\\", true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertEquals("file://server/share/", result.urlString());
    assertNotNull(result.uri());
    assertEquals("file", result.uri().getScheme());
    assertEquals("server", result.uri().getHost());
    assertEquals("/share/", result.uri().getPath());
    assertNull(result.errorMessage());
  }

  @Test
  @EnabledOnOs({org.junit.jupiter.api.condition.OS.WINDOWS, org.junit.jupiter.api.condition.OS.MAC})
  void testProcessWebLink_UncPath_WithIllegalChars()
  {
    // UNC path with illegal URI characters (spaces, brackets)
    UrlOpenResult result = processWebLink("\\\\server\\share\\folder [1]\\file.txt", true);

    assertEquals(UrlOpenAction.BROWSE_WEB, result.action());
    assertNotNull(result.uri());
    assertEquals("file", result.uri().getScheme());
    assertEquals("server", result.uri().getHost());
    // Path should have spaces and brackets escaped
    assertTrue(result.uri().getRawPath().contains("%20"), "Spaces should be escaped");
    assertTrue(result.uri().getRawPath().contains("%5B"), "Brackets should be escaped");
    assertNull(result.errorMessage());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
