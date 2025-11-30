sap.ui.define([
    "sap/ui/core/mvc/Controller",
    "sap/ui/model/json/JSONModel"
], function(Controller, JSONModel) {
    "use strict";

    return Controller.extend("com.example.chat.controller.Main", {
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
                });
            } else {
                // Устанавливаем модель в view для обеспечения привязки
                oView.setModel(oModel, "messages");
                
                // Проверяем данные
                var oData = oModel.getData();
                if (!oData || !oData.messages) {
                    // Модель не содержит массива messages
                }
            }
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
            sap.m.MessageToast.show("Нажата кнопка: " + sText + " (Key: " + sKey + ")");
        },

        onMessageListUpdateFinished: function() {
            // Автоматически прокручиваем вниз после обновления списка сообщений
            this._scrollToBottom();
        },

        onSendMessage: function() {
            var oInput = this.byId("messageInput");
            var sMessageText = oInput.getValue();
            
            if (sMessageText.trim() === "") {
                sap.m.MessageToast.show("Пожалуйста, введите сообщение");
                return;
            }

            var oModel = this.getView().getModel("messages");
            var aMessages = oModel.getData().messages || [];
            
            var oNewMessage = {
                sender: "Вы",
                text: sMessageText,
                timestamp: new Date().toLocaleString("ru-RU"),
                icon: "sap-icon://user",
                Actions: [
                    { Text: "Action 1", Key: "action1" }
                ]
            };
            
            aMessages.push(oNewMessage);
            oModel.setData({ messages: aMessages });
            oModel.refresh(true);
            
            // Очищаем поле ввода
            oInput.setValue("");
            
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

        onExit: function() {
            // Очищаем обработчики при уничтожении контроллера
            if (this._oResizeObserver) {
                this._oResizeObserver.disconnect();
                this._oResizeObserver = null;
            }
            jQuery(window).off("resize", this._calculateAndSetHeight);
        }
    });
});

