# Chat Application

A simple SAP UI5 web chat application that allows sending and displaying messages in real-time.

## Description

The application is a chat interface with automatic scrolling to new messages and dynamic calculation of message container height.

## Technologies

- **SAP UI5** (version 1.54.0+)
- **HTML5/CSS3**
- **JavaScript**

## Project Structure

```
gpt_test_b_chat/
├── Component.js          # Main application component
├── manifest.json         # SAP UI5 application manifest
├── index.html           # Application entry point
├── controller/
│   └── Chat.controller.js    # Controller for chat logic
├── view/
│   └── Chat.view.xml         # XML view for chat interface
├── model/
│   └── models.js             # Data models (JSONModel for messages)
└── css/
    └── style.css             # Application styles
```

## Main Features

- ✅ Display messages in FeedListItem format
- ✅ Send messages through input field
- ✅ Automatic scrolling to the last message
- ✅ Dynamic calculation of message container height
- ✅ Support for different sender types (user, system)
- ✅ Different icons for different sender types
- ✅ Send message with Ctrl+Enter
- ✅ Automatic TextArea resizing on input

## Database Tables

Dictionary for class YCL_LLM_CHAT_HANDLER

### YLLM_INTENTS
| Field | Type | Description |
|-------|------|-------------|
| MANDT | CLNT | Client |
| INTENT_NAME | CHAR30 | Intent name (key) |
| CATEGORY | CHAR30 | Category |
| CLASS_NAME | CHAR30 | Class name |
| DESCRIPTION | TEXT200 | Description |
| ACTIVE | FLAG | Active flag |

**Important:** To use tools functionality, you need to populate the `YLLM_INTENTS` table with intent definitions. Each intent should specify:
- `INTENT_NAME`: Unique name of the intent
- `CATEGORY`: Category the intent belongs to
- `CLASS_NAME`: Name of the class implementing the tool
- `DESCRIPTION`: Description of what the intent does
- `ACTIVE`: Flag indicating if the intent is active

### YLLM_MESSAGES
| Field | Type | Description |
|-------|------|-------------|
| MANDT | CLNT | Client |
| SESSION_ID | CHAR32 | Session ID (key) |
| MESSAGE_NUM | INT4 | Message number (key) |
| ROLE | CHAR20 | Message role (user/assistant/tool) |
| CONTENT | STRG | Message content |
| TIMESTAMP | TIMESTAMP | Message timestamp |
| TOOL_CALLS | STRG | Tool calls (JSON) |
| TOOL_CALL_ID | CHAR32 | Tool call ID |

### YLLM_SESSIONS
| Field | Type | Description |
|-------|------|-------------|
| MANDT | CLNT | Client |
| SESSION_ID | CHAR32 | Session ID (key) |
| START_TIMESTAMP | TIMESTAMP | Session start time |
| SAP_LOGON | USERNAME | SAP user login |
| NICKNAME | CHAR30 | User nickname |
| SESSION_TITLE | STRG | Session title |

### YLLM_USERS
| Field | Type | Description |
|-------|------|-------------|
| MANDT | CLNT | Client |
| SAP_LOGON | USERNAME | SAP user login (key) |
| NICKNAME | CHAR30 | User nickname (key) |
| LLM_NAME | TEXT60 | LLM name |
| GENDER | CHAR1 | Gender |
| LANGUAGE_PREF | LANG | Language preference |

## Setup

### Prerequisites

1. SAP system with required database tables created
2. ABAP classes deployed:
   - `YCL_LLM_CHAT_HANDLER`
   - `YCL_LLM_SERVICE`
   - `YCL_TOOL_CATALOG`

### SICF Service Configuration

**Configure the REST API handler service:**

1. Open transaction **SICF** (Service Maintenance)
2. Navigate to the node: `/default_host/sap/yllm`
3. If the node doesn't exist, create it:
   - Right-click on `/default_host/sap/` → Create → Service
   - Enter service name: `yllm`
4. Configure the service:
   - Set **Handler List**: `YCL_LLM_CHAT_HANDLER`
   - Set **Handler Type**: `CL_HTTP_EXTENSION`
   - Activate the service

**Note:** The application uses this service endpoint for backend communication (e.g., `/sap/yllm/chat/message`, `/sap/yllm/session`).

### Configuration

**To enable tools functionality:**

1. Populate the `YLLM_INTENTS` table with intent definitions
2. Each intent must have:
   - A unique `INTENT_NAME`
   - A `CATEGORY` (e.g., 'SAP_DATA', 'SYSTEM_INFO')
   - A `CLASS_NAME` that implements the tool interface
   - An `ACTIVE` flag set to 'X' to enable the intent

Example:
```
INTENT_NAME: 'QUERY_SAP_TABLE'
CATEGORY: 'SAP_DATA'
CLASS_NAME: 'YCL_SAP_TABLE_QUERY_TOOL'
DESCRIPTION: 'Query SAP database tables'
ACTIVE: 'X'
```

## Running

The application runs as a standard SAPUI5 application deployed via SICF service.

1. **Deploy the SAPUI5 application:**
   - Upload the application files to your SAP system
   - Create a SICF service for the UI5 application (transaction SICF)
   - Configure the service to point to the application files

2. **Access the application:**
   - Open the application URL in a browser (e.g., `https://your-sap-server:port/sap/bc/ui5_ui5/sap/your_app_name`)
   - The application will communicate with the backend service at `/sap/yllm`

**Important:** 
- The backend REST API service must be configured at `/default_host/sap/yllm` with handler `YCL_LLM_CHAT_HANDLER`
- The application is not designed to run on localhost - it requires the SAP backend service

## Usage

- Enter a message in the input field at the bottom of the screen
- Press the send button or use **Ctrl+Enter**
- Messages automatically scroll down
- Message container height automatically adjusts to window size

## Implementation Details

- **Dynamic Height**: ScrollContainer automatically calculates its height as the difference between available screen height and input panel height
- **Auto-scroll**: When a new message is added, the list automatically scrolls to the last element
- **Responsiveness**: The application adapts to browser window size changes
- **Timestamp Formatting**: SAP timestamps (YYYYMMDDHHMMSS format) are automatically formatted to readable format (DD.MM.YYYY, HH:MM:SS)

## License

MIT
