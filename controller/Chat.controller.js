sap.ui.define([
    "sap/ui/core/mvc/Controller",
    "sap/ui/model/json/JSONModel",
    "sap/m/MessageBox",
    "sap/m/MessageToast",
    "sap/m/Dialog",
    "sap/m/Input",
    "sap/m/Button",
    "sap/m/Text"
], function(Controller, JSONModel, MessageBox, MessageToast, Dialog, Input, Button, Text) {
    "use strict";

    return Controller.extend("com.example.chat.controller.Chat", {
        onInit: function() {
            var oView = this.getView();
            var oComponent = this.getOwnerComponent();
            
            // Получаем модель из компонента или создаем новую
            var oModel = oComponent.getModel("messages");
            
            if (!oModel) {
                // Если модель не найдена, создаем её
                sap.ui.require(["com/example/chat/model/models"], function(models) {
                    var oNewModel = models.createMessageModel();
                    oComponent.setModel(oNewModel, "messages");
                    oView.setModel(oNewModel, "messages");
                    
                    // Инициализируем данные пользователя
                    this._initializeUserData(oNewModel);
                }.bind(this));
            } else {
                // Устанавливаем модель в view для обеспечения привязки
                oView.setModel(oModel, "messages");
                
                // Инициализируем данные пользователя
                this._initializeUserData(oModel);
            }
        },
        
        /**
         * Инициализирует данные пользователя и загружает историю чата
         * @param {sap.ui.model.json.JSONModel} oModel - Модель данных
         */
        _initializeUserData: function(oModel) {
            // Получаем данные пользователя из SAP (можно получить из контекста или настроек)
            var sSapLogon = this._getSapLogon();
            var sNickname = this._getNickname();
            var sSessionId = this._getSessionId();
            
            var oData = oModel.getData();
            oData.sapLogon = sSapLogon;
            oData.nickname = sNickname;
            oData.sessionId = sSessionId;
            oData.sessions = oData.sessions || [];
            oModel.setData(oData);
            
            // Загружаем список сессий
            this._loadUserSessions();
            
            // Загружаем историю чата, если есть session_id
            if (sSessionId) {
                this._loadChatHistory(sSessionId, sSapLogon, sNickname);
            }
        },
        
        /**
         * Получает SAP логин пользователя
         * @returns {string} SAP логин
         */
        _getSapLogon: function() {
            // Можно получить из контекста SAP или настроек
            // Пока используем значение по умолчанию или из localStorage
            return localStorage.getItem("sapLogon") || "";
        },
        
        /**
         * Получает никнейм пользователя
         * @returns {string} Никнейм
         */
        _getNickname: function() {
            // Можно получить из контекста SAP или настроек
            return localStorage.getItem("nickname") || "Пользователь";
        },
        
        /**
         * Получает ID текущей сессии
         * @returns {string} ID сессии
         */
        _getSessionId: function() {
            // Можно получить из URL параметров или localStorage
            var sUrlParams = new URLSearchParams(window.location.search);
            return sUrlParams.get("session_id") || localStorage.getItem("sessionId") || null;
        },
        
        /**
         * Загружает историю чата из SAP
         * @param {string} sSessionId - ID сессии
         * @param {string} sSapLogon - SAP логин
         * @param {string} sNickname - Никнейм
         */
        _loadChatHistory: function(sSessionId, sSapLogon, sNickname) {
            var oModel = this.getView().getModel("messages");
            var that = this;
            
            sap.ui.require(["com/example/chat/model/models"], function(models) {
                models.loadChatHistory(sSessionId, sSapLogon, sNickname).then(function(aHistory) {
                    // Преобразуем историю в формат для отображения
                    var aMessages = that._convertHistoryToMessages(aHistory);
                    var oData = oModel.getData();
                    oData.messages = aMessages;
                    oModel.setData(oData);
                    oModel.refresh(true);
                    
                    // Прокручиваем к последнему сообщению
                    setTimeout(function() {
                        that._scrollToBottom();
                    }, 100);
                }).catch(function(oError) {
                    console.error("Ошибка загрузки истории:", oError);
                });
            });
        },
        
        /**
         * Форматирует timestamp в читаемый формат
         * @param {string|number|object} sTimestamp - Timestamp в различных форматах (ISO, SAP timestamp, и т.д.)
         * @returns {string} Отформатированная дата и время
         */
        _formatTimestamp: function(sTimestamp) {
            if (!sTimestamp && sTimestamp !== 0) {
                return new Date().toLocaleString("ru-RU");
            }
            
            // Логируем входные данные для отладки
            console.log("_formatTimestamp вызвана с:", sTimestamp, "тип:", typeof sTimestamp);
            
            var oDate;
            
            // Пытаемся распарсить timestamp
            try {
                // Если это уже объект Date
                if (sTimestamp instanceof Date) {
                    oDate = sTimestamp;
                }
                // Если это объект (может быть из JSON)
                else if (typeof sTimestamp === "object" && sTimestamp !== null) {
                    // Пытаемся извлечь значение из объекта
                    if (sTimestamp.value) {
                        sTimestamp = sTimestamp.value;
                    } else if (sTimestamp.timestamp) {
                        sTimestamp = sTimestamp.timestamp;
                    } else {
                        // Пытаемся преобразовать объект в строку
                        sTimestamp = JSON.stringify(sTimestamp);
                    }
                    // Рекурсивно вызываем для преобразованного значения
                    return this._formatTimestamp(sTimestamp);
                }
                // Если это число
                else if (typeof sTimestamp === "number") {
                    // Преобразуем число в строку для проверки
                    var sTimestampStr = String(sTimestamp);
                    
                    // Проверяем, является ли это SAP timestamp формата YYYYMMDDHHMMSS (14 цифр)
                    // SAP timestamp как число будет иметь длину 14 символов
                    if (sTimestampStr.length === 14 && /^\d{14}$/.test(sTimestampStr)) {
                        // SAP timestamp: 20240101120000
                        var sYear = sTimestampStr.substring(0, 4);
                        var sMonth = sTimestampStr.substring(4, 6);
                        var sDay = sTimestampStr.substring(6, 8);
                        var sHour = sTimestampStr.substring(8, 10);
                        var sMinute = sTimestampStr.substring(10, 12);
                        var sSecond = sTimestampStr.substring(12, 14);
                        oDate = new Date(
                            parseInt(sYear, 10),
                            parseInt(sMonth, 10) - 1,
                            parseInt(sDay, 10),
                            parseInt(sHour, 10),
                            parseInt(sMinute, 10),
                            parseInt(sSecond, 10)
                        );
                        console.log("Обработан SAP timestamp (число):", sTimestamp, "->", oDate);
                    } else {
                        // Если это не SAP timestamp, считаем миллисекундами с 1970
                        oDate = new Date(sTimestamp);
                    }
                }
                // Если это строка
                else if (typeof sTimestamp === "string") {
                    var sTimestampStr = sTimestamp.trim();
                    
                    // Если пустая строка
                    if (sTimestampStr === "") {
                        return new Date().toLocaleString("ru-RU");
                    }
                    
                    // Проверяем, является ли это SAP timestamp формата YYYYMMDDHHMMSS (14 цифр)
                    if (/^\d{14}$/.test(sTimestampStr)) {
                        // SAP timestamp: 20240101120000
                        var sYear = sTimestampStr.substring(0, 4);
                        var sMonth = sTimestampStr.substring(4, 6);
                        var sDay = sTimestampStr.substring(6, 8);
                        var sHour = sTimestampStr.substring(8, 10);
                        var sMinute = sTimestampStr.substring(10, 12);
                        var sSecond = sTimestampStr.substring(12, 14);
                        oDate = new Date(
                            parseInt(sYear, 10),
                            parseInt(sMonth, 10) - 1,
                            parseInt(sDay, 10),
                            parseInt(sHour, 10),
                            parseInt(sMinute, 10),
                            parseInt(sSecond, 10)
                        );
                        console.log("Обработан SAP timestamp (строка):", sTimestampStr, "->", oDate);
                    }
                    // Проверяем формат YYYYMMDDHHMMSS с точками или дефисами (например, 2024.01.01 12:00:00 или 2024-01-01T12:00:00)
                    else if (/^\d{4}[.\-]\d{2}[.\-]\d{2}[T\s]\d{2}[:]\d{2}[:]\d{2}/.test(sTimestampStr)) {
                        // Заменяем точки на дефисы и пробелы на T
                        var sNormalized = sTimestampStr.replace(/\./g, "-").replace(/\s+/, "T");
                        // Добавляем Z если нет временной зоны
                        if (!sNormalized.includes("Z") && !sNormalized.match(/[+-]\d{2}:\d{2}$/)) {
                            sNormalized += "Z";
                        }
                        oDate = new Date(sNormalized);
                    }
                    // Проверяем формат YYYY-MM-DDTHH:MM:SS или похожий ISO формат
                    else if (/^\d{4}-\d{2}-\d{2}[T\s]\d{2}:\d{2}:\d{2}/.test(sTimestampStr)) {
                        var sIsoTimestamp = sTimestampStr.replace(/\s+/, "T");
                        // Добавляем Z если нет временной зоны
                        if (!sIsoTimestamp.includes("Z") && !sIsoTimestamp.match(/[+-]\d{2}:\d{2}$/)) {
                            sIsoTimestamp += "Z";
                        }
                        oDate = new Date(sIsoTimestamp);
                    }
                    // Пытаемся распарсить как ISO или другой стандартный формат
                    else {
                        oDate = new Date(sTimestampStr);
                    }
                }
                else {
                    // Для других типов пытаемся преобразовать в строку
                    oDate = new Date(String(sTimestamp));
                }
                
                // Проверяем, что дата валидна
                if (!oDate || isNaN(oDate.getTime())) {
                    // Если не удалось распарсить, логируем и возвращаем исходное значение
                    console.warn("Не удалось распарсить timestamp:", sTimestamp, typeof sTimestamp);
                    return String(sTimestamp || "");
                }
                
                // Форматируем дату в русском формате
                var sFormatted = oDate.toLocaleString("ru-RU", {
                    year: "numeric",
                    month: "2-digit",
                    day: "2-digit",
                    hour: "2-digit",
                    minute: "2-digit",
                    second: "2-digit"
                });
                console.log("Timestamp отформатирован:", sTimestamp, "->", sFormatted);
                return sFormatted;
            } catch (e) {
                // В случае ошибки возвращаем исходное значение
                console.warn("Ошибка форматирования timestamp:", sTimestamp, typeof sTimestamp, e);
                return String(sTimestamp || "");
            }
        },
        
        /**
         * Преобразует историю из формата SAP в формат для отображения
         * @param {Array} aHistory - История из SAP
         * @returns {Array} Массив сообщений для отображения
         */
        _convertHistoryToMessages: function(aHistory) {
            if (!aHistory || !Array.isArray(aHistory)) {
                return [];
            }
            
            var that = this;
            return aHistory.map(function(oMsg) {
                var sRole = oMsg.role || "";
                var sSender = "";
                var sIcon = "";
                
                if (sRole === "user") {
                    sSender = "Вы";
                    sIcon = "sap-icon://user";
                } else if (sRole === "assistant") {
                    sSender = "Ассистент";
                    sIcon = "sap-icon://robot";
                } else if (sRole === "tool") {
                    sSender = "Система";
                    sIcon = "sap-icon://tools-opportunity";
                } else {
                    sSender = "Система";
                    sIcon = "sap-icon://cloud";
                }
                
                // Форматируем timestamp - обязательно применяем форматирование
                var sFormattedTimestamp = that._formatTimestamp(oMsg.timestamp);
                
                // Логируем для отладки (можно убрать после проверки)
                if (oMsg.timestamp && oMsg.timestamp !== sFormattedTimestamp) {
                    console.log("Timestamp отформатирован:", {
                        original: oMsg.timestamp,
                        formatted: sFormattedTimestamp,
                        type: typeof oMsg.timestamp
                    });
                }
                
                return {
                    sender: sSender,
                    text: oMsg.content || "",
                    timestamp: sFormattedTimestamp,
                    icon: sIcon,
                    Actions: []
                };
            });
        },

        onAfterRendering: function() {
            // Добавляем обработчик для отправки по Ctrl+Enter после рендеринга
            var oTextArea = this.byId("messageInput");
            if (oTextArea) {
                oTextArea.attachBrowserEvent("keydown", function(oEvent) {
                    if (oEvent.ctrlKey && oEvent.keyCode === 13) { // Ctrl+Enter
                        oEvent.preventDefault();
                        this.onSendMessage();
                    }
                }.bind(this));
                
                // Пересчитываем высоту при изменении размера TextArea
                oTextArea.attachLiveChange(function() {
                    setTimeout(function() {
                        this._calculateAndSetHeight();
                    }.bind(this), 50);
                }.bind(this));
            }
            
            // Вычисляем и устанавливаем высоту ScrollContainer
            this._calculateAndSetHeight();
            
            // Добавляем обработчик изменения размера окна с debounce
            var oResizeTimeout;
            var fnResizeHandler = function() {
                clearTimeout(oResizeTimeout);
                oResizeTimeout = setTimeout(function() {
                    this._calculateAndSetHeight();
                }.bind(this), 150);
            }.bind(this);
            
            if (window.ResizeObserver) {
                var oView = this.getView();
                var oViewDomRef = oView.getDomRef();
                if (oViewDomRef) {
                    this._oResizeObserver = new ResizeObserver(fnResizeHandler);
                    this._oResizeObserver.observe(oViewDomRef);
                }
            } else {
                // Fallback для старых браузеров
                jQuery(window).on("resize", fnResizeHandler);
            }
            
            // Прокручиваем к последнему сообщению при первой загрузке
            setTimeout(function() {
                this._scrollToBottom();
            }.bind(this), 100);
        },

        onActionPressed: function(oEvent) {
            var oAction = oEvent.getSource();
            var sKey = oAction.getKey();
            var sText = oAction.getText();
                        MessageToast.show("Нажата кнопка: " + sText + " (Key: " + sKey + ")");
        },

        onMessageListUpdateFinished: function() {
            // Автоматически прокручиваем вниз после обновления списка сообщений
            this._scrollToBottom();
        },

        onSendMessage: function() {
            var oInput = this.byId("messageInput");
            var sMessageText = oInput.getValue();
            
            if (sMessageText.trim() === "") {
                MessageToast.show("Пожалуйста, введите сообщение");
                return;
            }

            var oModel = this.getView().getModel("messages");
            var oData = oModel.getData();
            var aMessages = oData.messages || [];
            var sSapLogon = oData.sapLogon || this._getSapLogon();
            var sNickname = oData.nickname || this._getNickname();
            var sSessionId = oData.sessionId || this._getSessionId();
            
            // Добавляем сообщение пользователя в UI сразу
            var oUserMessage = {
                sender: "Вы",
                text: sMessageText,
                timestamp: new Date().toLocaleString("ru-RU"),
                icon: "sap-icon://user",
                Actions: []
            };
            
            aMessages.push(oUserMessage);
            oModel.setData({ messages: aMessages });
            oModel.refresh(true);
            
            // Очищаем поле ввода
            oInput.setValue("");
            
            // Отправляем сообщение в SAP
            var that = this;
            sap.ui.require(["com/example/chat/model/models"], function(models) {
                models.sendMessageToSAP(sMessageText, sSessionId, sSapLogon, sNickname)
                    .then(function(oResponse) {
                        // Обновляем session_id, если он был создан
                        if (oResponse.session_id && !oData.sessionId) {
                            oData.sessionId = oResponse.session_id;
                            localStorage.setItem("sessionId", oResponse.session_id);
                            oModel.setData(oData);
                        }
                        
                        // Добавляем ответ ассистента
                        if (oResponse.message) {
                            var oAssistantMessage = {
                                sender: "Ассистент",
                                text: oResponse.message,
                                timestamp: new Date().toLocaleString("ru-RU"),
                                icon: "sap-icon://robot",
                                Actions: []
                            };
                            
                            var oCurrentData = oModel.getData();
                            var aCurrentMessages = oCurrentData.messages || [];
                            aCurrentMessages.push(oAssistantMessage);
                            oCurrentData.messages = aCurrentMessages;
                            oModel.setData(oCurrentData);
                            oModel.refresh(true);
                            
                            // Прокручиваем к последнему сообщению
                            setTimeout(function() {
                                that._scrollToBottom();
                            }, 100);
                        }
                    })
                    .catch(function(oError) {
                        // Показываем ошибку пользователю
                        var sErrorMsg = "Ошибка отправки сообщения";
                        if (oError.responseJSON && oError.responseJSON.error) {
                            sErrorMsg = oError.responseJSON.error;
                        } else if (oError.responseText) {
                            try {
                                var oErrorData = JSON.parse(oError.responseText);
                                sErrorMsg = oErrorData.error || sErrorMsg;
                            } catch (e) {
                                sErrorMsg = oError.responseText || sErrorMsg;
                            }
                        }
                        
                        MessageBox.error(sErrorMsg);
                        
                        // Удаляем сообщение пользователя из списка при ошибке (опционально)
                        // var oErrorData = oModel.getData();
                        // var aErrorMessages = oErrorData.messages || [];
                        // aErrorMessages.pop(); // Удаляем последнее сообщение
                        // oErrorData.messages = aErrorMessages;
                        // oModel.setData(oErrorData);
                    });
            });
            
            // Пересчитываем высоту после очистки TextArea
            setTimeout(function() {
                this._calculateAndSetHeight();
                this._scrollToBottom();
            }.bind(this), 100);
        },

        _calculateAndSetHeight: function() {
            var oView = this.getView();
            var oViewDomRef = oView.getDomRef();
            var oScrollContainer = this.byId("scrollContainer");
            var oToolbar = this.byId("messageInput") ? this.byId("messageInput").getParent() : null;
            
            if (!oViewDomRef || !oScrollContainer || !oToolbar) {
                return;
            }
            
            // Получаем доступную высоту view или окна
            var iAvailableHeight = oViewDomRef.clientHeight;
            if (!iAvailableHeight || iAvailableHeight === 0) {
                iAvailableHeight = window.innerHeight;
            }
            
            // Устанавливаем высоту view на 100vh, если она еще не установлена
            if (!oViewDomRef.style.height || oViewDomRef.style.height === "") {
                jQuery(oViewDomRef).css("height", "100vh");
            }
            
            // Получаем высоту Toolbar с TextArea
            var oToolbarDomRef = oToolbar.getDomRef();
            var iToolbarHeight = 0;
            if (oToolbarDomRef) {
                iToolbarHeight = oToolbarDomRef.offsetHeight;
            }
            
            // Получаем отступы VerticalLayout (sapUiContentPadding)
            var oVerticalLayout = this.byId("verticalLayout");
            var iPadding = 0;
            if (oVerticalLayout) {
                var oVerticalLayoutDomRef = oVerticalLayout.getDomRef();
                if (oVerticalLayoutDomRef) {
                    var oStyles = window.getComputedStyle(oVerticalLayoutDomRef);
                    iPadding = parseInt(oStyles.paddingTop || 0, 10) + parseInt(oStyles.paddingBottom || 0, 10);
                }
            }
            
            // Вычисляем высоту для ScrollContainer
            // Доступная высота минус высота Toolbar и отступы
            var iScrollContainerHeight = iAvailableHeight - iToolbarHeight - iPadding;
            
            // Устанавливаем высоту в пикселях
            if (iScrollContainerHeight > 0) {
                oScrollContainer.setHeight(iScrollContainerHeight + "px");
            }
        },

        _scrollToBottom: function() {
            var oScrollContainer = this.byId("scrollContainer");
            if (oScrollContainer) {
                var oDomRef = oScrollContainer.getDomRef();
                if (oDomRef) {
                    // Прокручиваем к нижней части ScrollContainer
                    oDomRef.scrollTop = oDomRef.scrollHeight;
                }
            }
        },
        
        /**
         * Загружает список сессий пользователя
         */
        _loadUserSessions: function() {
            var oModel = this.getView().getModel("messages");
            var oData = oModel.getData();
            var sSapLogon = oData.sapLogon || this._getSapLogon();
            var sNickname = oData.nickname || this._getNickname();
            var that = this;
            
            if (!sSapLogon || !sNickname) {
                return;
            }
            
            sap.ui.require(["com/example/chat/model/models"], function(models) {
                models.getUserSessions(sSapLogon, sNickname)
                    .then(function(aSessions) {
                        var oCurrentData = oModel.getData();
                        oCurrentData.sessions = aSessions || [];
                        oModel.setData(oCurrentData);
                        oModel.refresh(true);
                        
                        // Устанавливаем выбранную сессию в селекторе
                        var oSessionSelect = that.byId("sessionSelect");
                        if (oSessionSelect && oCurrentData.sessionId) {
                            oSessionSelect.setSelectedKey(oCurrentData.sessionId);
                        }
                    })
                    .catch(function(oError) {
                        console.error("Ошибка загрузки сессий:", oError);
                    });
            });
        },
        
        /**
         * Обработчик создания новой сессии
         */
        onCreateSession: function() {
            var that = this;
            var oModel = this.getView().getModel("messages");
            var oData = oModel.getData();
            var sSapLogon = oData.sapLogon || this._getSapLogon();
            var sNickname = oData.nickname || this._getNickname();
            
            // Если нет логина или никнейма, открываем диалог настроек
            if (!sSapLogon || !sNickname) {
                MessageBox.confirm(
                    "Для создания сессии необходимо указать SAP логин и никнейм. Открыть настройки?",
                    {
                        title: "Требуются настройки",
                        icon: MessageBox.Icon.QUESTION,
                        actions: [MessageBox.Action.OK, MessageBox.Action.CANCEL],
                        onClose: function(sAction) {
                            if (sAction === "OK") {
                                that.onOpenSettings();
                            }
                        }
                    }
                );
                return;
            }
            
            // Запрашиваем название сессии у пользователя
            if (!this._oSessionNameDialog) {
                this._oSessionNameInput = new Input({
                    placeholder: "Введите название новой сессии (необязательно)",
                    value: "Новая сессия"
                });
                
                this._oSessionNameDialog = new Dialog({
                    title: "Создание сессии",
                    content: [
                        new Text({
                            text: "Введите название новой сессии (необязательно)"
                        }),
                        this._oSessionNameInput
                    ],
                    beginButton: new Button({
                        text: "Создать",
                        type: "Emphasized",
                        press: function() {
                            var sSessionTitle = that._oSessionNameInput.getValue() || "Новая сессия";
                            that._oSessionNameDialog.close();
                            that._createSession(sSapLogon, sNickname, sSessionTitle);
                        }
                    }),
                    endButton: new Button({
                        text: "Отмена",
                        press: function() {
                            that._oSessionNameDialog.close();
                        }
                    })
                });
            } else {
                // Сбрасываем значение при повторном открытии
                this._oSessionNameInput.setValue("Новая сессия");
            }
            this._oSessionNameDialog.open();
        },
        
        /**
         * Создает новую сессию
         * @param {string} sSapLogon - SAP логин
         * @param {string} sNickname - Никнейм
         * @param {string} sSessionTitle - Заголовок сессии
         */
        _createSession: function(sSapLogon, sNickname, sSessionTitle) {
            var that = this;
            var oModel = this.getView().getModel("messages");
            
            sap.ui.require(["com/example/chat/model/models"], function(models) {
                models.createSession(sSapLogon, sNickname, sSessionTitle)
                    .then(function(oResponse) {
                        if (oResponse.session_id) {
                            // Обновляем текущую сессию
                            var oData = oModel.getData();
                            oData.sessionId = oResponse.session_id;
                            oData.messages = []; // Очищаем сообщения для новой сессии
                            oModel.setData(oData);
                            
                            // Сохраняем в localStorage
                            localStorage.setItem("sessionId", oResponse.session_id);
                            
                            // Обновляем список сессий
                            that._loadUserSessions();
                            
                            // Устанавливаем выбранную сессию
                            var oSessionSelect = that.byId("sessionSelect");
                            if (oSessionSelect) {
                                oSessionSelect.setSelectedKey(oResponse.session_id);
                            }
                            
                            MessageToast.show("Сессия создана успешно");
                        }
                    })
                    .catch(function(oError) {
                        var sErrorMsg = "Ошибка создания сессии";
                        if (oError.responseJSON && oError.responseJSON.error) {
                            sErrorMsg = oError.responseJSON.error;
                        }
                        MessageBox.error(sErrorMsg);
                    });
            });
        },
        
        /**
         * Обработчик обновления списка сессий
         */
        onRefreshSessions: function() {
            this._loadUserSessions();
            MessageToast.show("Список сессий обновлен");
        },
        
        /**
         * Обработчик изменения выбранной сессии
         * @param {sap.ui.base.Event} oEvent - Событие изменения
         */
        onSessionChange: function(oEvent) {
            var sSelectedSessionId = oEvent.getParameter("selectedItem").getKey();
            var oModel = this.getView().getModel("messages");
            var oData = oModel.getData();
            
            // Обновляем текущую сессию
            oData.sessionId = sSelectedSessionId;
            oData.messages = []; // Очищаем сообщения при переключении сессии
            oModel.setData(oData);
            
            // Сохраняем в localStorage
            localStorage.setItem("sessionId", sSelectedSessionId);
            
            // Загружаем историю для выбранной сессии
            var sSapLogon = oData.sapLogon || this._getSapLogon();
            var sNickname = oData.nickname || this._getNickname();
            if (sSelectedSessionId) {
                this._loadChatHistory(sSelectedSessionId, sSapLogon, sNickname);
            }
        },

        /**
         * Открывает диалог настроек
         */
        onOpenSettings: function() {
            var oDialog = this.byId("settingsDialog");
            var oModel = this.getView().getModel("messages");
            var oData = oModel.getData();
            
            // Заполняем поля текущими значениями
            var oSapLogonInput = this.byId("sapLogonInput");
            var oNicknameInput = this.byId("nicknameInput");
            
            if (oSapLogonInput) {
                oSapLogonInput.setValue(oData.sapLogon || this._getSapLogon() || "");
            }
            if (oNicknameInput) {
                oNicknameInput.setValue(oData.nickname || this._getNickname() || "");
            }
            
            if (oDialog) {
                oDialog.open();
            }
        },
        
        /**
         * Сохраняет настройки пользователя
         */
        onSaveSettings: function() {
            var oSapLogonInput = this.byId("sapLogonInput");
            var oNicknameInput = this.byId("nicknameInput");
            var oModel = this.getView().getModel("messages");
            
            var sSapLogon = oSapLogonInput ? oSapLogonInput.getValue().trim() : "";
            var sNickname = oNicknameInput ? oNicknameInput.getValue().trim() : "";
            
            if (!sSapLogon || !sNickname) {
                MessageBox.alert("Пожалуйста, заполните все обязательные поля", {
                    title: "Ошибка",
                    icon: MessageBox.Icon.ERROR
                });
                return;
            }
            
            // Сохраняем в модель
            var oData = oModel.getData();
            oData.sapLogon = sSapLogon;
            oData.nickname = sNickname;
            oModel.setData(oData);
            
            // Сохраняем в localStorage
            localStorage.setItem("sapLogon", sSapLogon);
            localStorage.setItem("nickname", sNickname);
            
            // Закрываем диалог
            this.onCloseSettings();
            
            // Обновляем список сессий
            this._loadUserSessions();
            
            MessageToast.show("Настройки сохранены");
        },
        
        /**
         * Закрывает диалог настроек
         */
        onCloseSettings: function() {
            var oDialog = this.byId("settingsDialog");
            if (oDialog) {
                oDialog.close();
            }
        },

        /**
         * Кнопка отладки: сбрасывает модель и выполняет инициализацию заново
         */
        onDebugReset: function() {
            var oView = this.getView();
            var oComponent = this.getOwnerComponent();
            
            // Удаляем модель из компонента и view
            oComponent.setModel(null, "messages");
            oView.setModel(null, "messages");
            
            // Выполняем логику инициализации с пустой моделью (ветка if (!oModel))
            sap.ui.require(["com/example/chat/model/models"], function(models) {
                var oNewModel = models.createMessageModel();
                oComponent.setModel(oNewModel, "messages");
                oView.setModel(oNewModel, "messages");
                
                // Инициализируем данные пользователя
                this._initializeUserData(oNewModel);
                
                MessageToast.show("Модель сброшена и инициализирована заново");
            }.bind(this));
        },

        onExit: function() {
            // Очищаем обработчики при уничтожении контроллера
            if (this._oResizeObserver) {
                this._oResizeObserver.disconnect();
                this._oResizeObserver = null;
            }
            jQuery(window).off("resize", this._calculateAndSetHeight);
            
            // Уничтожаем диалог создания сессии (Input уничтожится автоматически как дочерний элемент)
            if (this._oSessionNameDialog) {
                this._oSessionNameDialog.destroy();
                this._oSessionNameDialog = null;
                this._oSessionNameInput = null;
            }
        }
    });
});

